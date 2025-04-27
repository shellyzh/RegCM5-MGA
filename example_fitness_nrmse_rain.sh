#!/bin/bash

## /////////////////////////////////////////////////////////////////////////////
set -e ## Stop on error
ulimit -s unlimited

cd {your_working_dir}

## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
## Read this run parameters here
source param.sh

DATE=2007090200     ### VERY IMPORTANT:P Currently skip first month

regcmDIR=regcm5-bin
LOG=log.scr.0.rcm_Gen${GEN}_IDV${IDV}.sh

FITFILE=regcm5-gafitness/fitness_gen${GEN}_idv${IDV}.txt

## /////////////////////////////////////////////////////////////////////////////
echo "Running log" > $LOG

echo "Checking Individual $IDV with binary code $IDVcode"
runcheck=` grep $IDVcode ga.run.log | tail -1 `
[[ ! -z $runcheck ]] && \
{
  echo "> Reuse fitness from previous "
  echo "Reuse fitness from previous " >  $FITFILE
  echo $runcheck | awk '{print $NF}'  >> $FITFILE
  echo "YES" > is.modelrun
  exit
}

echo "> Starting simulation of Gen $GEN and individual $IDV"  >> $LOG

echo "YES" > is.modelrun


## -----------------------------------------------------------------------------
[[ ! -z $DOM1 ]] && \
{
  echo ">   Performing for the 1st domain with $NML1" >> $LOG
  mpirun -n $NCORES $regcmDIR/regcmMPICLM45 $NML1 >> $LOG
}

## -----------------------------------------------------------------------------
[[ ! -z $DOM2 ]] && \
{
  echo ">   Performing for the 2nd domain with $NML2" >> $LOG  
  mpirun -n $NCORES $regcmDIR/regcmMPICLM45 $NML2 >> $LOG
}

## -----------------------------------------------------------------------------
[[ ! -z $DOM3 ]] && \
{
  echo ">   Performing for the 3rd domain with $NML3" >> $LOG
  mpirun -n $NCORES $regcmDIR/regcmMPICLM45 $NML3 >> $LOG
}





## /////////////////////////////////////////////////////////////////////////////
## Calculating fitness
## quick calculate for only D1
MDLLANDMASK=obsfile/landmask_RegCM.nc
MDLFILE=regcm5-out/gen${GEN}_idv${IDV}/${DOM1}_STS.${DATE}.nc
#### these will be removed
MDLRAIN=/dev/shm/_ga_mdl_rain_gen${GEN}_idv${IDV}_
MDLRAIN_TIMMEAN=/dev/shm/_ga_mdl_rain_timmean_gen${GEN}_idv${IDV}_

#### these will be removed
TIMRMSERAIN=fitness_intermediate/_ga_mdl_timrmse_rain_gen${GEN}_idv${IDV}_
N_TIMRMSERAIN=fitness_intermediate/_ga_mdl_n_timrmse_rain_gen${GEN}_idv${IDV}_
FLDRMSERAIN=fitness_intermediate/_ga_mdl_fldrmse_rain_gen${GEN}_idv${IDV}_
N_FLDRMSERAIN=fitness_intermediate/_ga_mdl_n_fldrmse_rain_gen${GEN}_idv${IDV}_

OBSRAIN=obsfile/observation_rain_${DATE}.nc # in mm/day
OBSRAIN_TIMMEAN=obsfile/observation_rain_timmean_${DATE}.nc
OBSRAIN_TIMSTD=obsfile/observation_rain_timstd_${DATE}.nc
OBSRAIN_FLDSTD=obsfile/observation_rain_fldstd_${DATE}.nc

FITFILE=regcm5-gafitness/fitness_gen${GEN}_idv${IDV}.txt

#echo $MDLFILE
#echo $OBSFILE

# extract from model: precipitation in mm/day
cdo -L -O -sellonlatbox,94,132,-9.5,8 \
          -seltimestep,2/3 \
          -remapbil,$OBSRAIN \
          -shifttime,-12hour \
          -setname,precip \
          -mulc,86400 -selvar,pr \
          $MDLFILE $MDLRAIN

# Calculate TIMNRMSE (TIMRMSE/OBS[VAR]_TIMSTD) and extract it
# 1: calculate TIMRMSE
# for precipitation
cdo -L -O -sqrt \
          -timmean \
          -sqr -sub \
          $MDLRAIN $OBSRAIN $TIMRMSERAIN
          
# 2: calculate N_TIMNRMSE
# for precipitation
cdo -L -O -fldmean \
          -div \
          $TIMRMSERAIN $OBSRAIN_TIMSTD $N_TIMRMSERAIN
          
# 3: calculate FLDRMSE
cdo -L -O -timmean \
          $MDLRAIN $MDLRAIN_TIMMEAN

cdo -L -O -sqrt \
          -fldmean \
          -sqr -sub \
          $MDLRAIN_TIMMEAN $OBSRAIN_TIMMEAN $FLDRMSERAIN

# 4: calculate N_FLDRMSE
cdo -L -O -div \
          $FLDRMSERAIN $OBSRAIN_FLDSTD $N_FLDRMSERAIN
          
# 5: add up TIMNRMSE and FLDNRMSE of RAIN, and output as fitness value
cdo -L -s -outputtab,value \
          -add  \
          $N_TIMRMSERAIN $N_FLDRMSERAIN > $FITFILE

rm -f /dev/shm/_ga_mdl_rain_gen${GEN}_idv${IDV}_
rm -f /dev/shm/_ga_mdl_rain_timmean_gen${GEN}_idv${IDV}_
rm -f fitness_intermediate/_ga_mdl_timrmse_rain_gen${GEN}_idv${IDV}_
rm -f fitness_intermediate/_ga_mdl_n_timrmse_rain_gen${GEN}_idv${IDV}_
rm -f fitness_intermediate/_ga_mdl_fldrmse_rain_gen${GEN}_idv${IDV}_
rm -f fitness_intermediate/_ga_mdl_n_fldrmse_rain_gen${GEN}_idv${IDV}_

### EOF
