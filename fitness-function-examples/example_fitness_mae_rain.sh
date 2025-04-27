#!/bin/bash

## /////////////////////////////////////////////////////////////////////////////
set -e ## Stop on error
ulimit -s unlimited

cd {your_working_dir}

## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
## Read this run parameters here
source param.sh

DATE=2007090200     ### VERY IMPORTANT:P Currently skip first day

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
MDLFILE=regcm5-out/gen${GEN}_idv${IDV}/${DOM1}_STS.${DATE}.nc
#### these will be removed
MDLRAIN=/dev/shm/_ga_mdl_rain_gen${GEN}_idv${IDV}_

OBSRAIN=obsfile/observation_rain_${DATE}.nc # in mm/day

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

# Calculate MAE and extract it
cdo -L -s -outputtab,value \
          -timmean  \
          -fldmean  \
          -abs -sub \
          $MDLRAIN $OBSRAIN > $FITFILE

rm -f /dev/shm/_ga_mdl_rain_gen${GEN}_idv${IDV}_

### EOF
