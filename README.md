# RegCM5-MGA
These are codes for an optimization framework RegCM5-MGA that couples the fifth version of Regional Climate Model (RegCM5) with a microgenetic algorithm (MGA). It is specifically used for optimizing five Tiedtke scheme parameters over one selected domain and one (continuous) timeframe. For the parameter details, please refer to the paper.

## General Usage Instructions

**ga.namelist.in** specifies the set up of RegCM5-MGA, users should update them and recompile the program

**mpihost** files are for job submissions (serverName:numberOfCores). The authors executed the programs based on GNU/Linux system, and the jobs are distributed to five mpihosts. Please ensure you have adapted the program to your system before running.

**prog.f90** files are for program compilation. After modifying any .f90 files, compile by running this command (*runga* can be replaced by other names that you desire):
ifort prog.main.f90 -o runga.exe

Among the .f90 files, please replace **{your_working_dir}** with the path to your working directory, and replace **{your_fitness_function}** with the path to your defined fitness functions

Two examples are included in *fitness-function-examples*, one for mean absolute error (MAE) based on precipitation values, and one for normalized root mean square error (NRMSE) based on precipitation values. Replace the directory of observation files with your own data storage directory.

Contact information:
Dr.Thanh Nguyen-Xuan: nxthanhnx@gmail.com
Ms. Zixuan Zhou: zzhoubh@connect.ust.hk
Professor Eun-Soon Im: ceim@ust.hk


--------------------------------------------
# TNX sugguestion
## General Usage Instructions

## Prerequisites
- The main program of RegCM5-MGA is developed in **Fortran 90**.
- It is designed to function optimally with any version of the **Intel compiler** released after 2016.
- To compile the program, let’s use command:
			***ifort prog.main.f90 -o run-ga.exe***   (run-ga.exe can be replaced by other names that you desire)
- Then, the Coupled RegCM5-MGA can be run by executing file ”***run-ga.exe***” (i.e., ***./run-ga.exe*** on linux)


## Program configuration
- Before executing the RegCM5-MGA (i.e., ***./run-ga.exe***), users are required to carefully configure each of the settings in the “**ga.namelist.in**” file, which specifies the setup of RegCM5-MGA. This configuration file is crucial as it contains all the parameters necessary for both MGA and RegCM5 configurations, ensuring the tool runs effectively according to your specific requirements.

- **mpihost\* files*** are for job submissions (serverName:numberOfCores). The authors executed the programs based on GNU/Linux system, and the jobs are distributed to five mpihosts. Please ensure you have adapted the program to your system before running.

## Notes
1. **prog.\*f90** files are for program compilation. After modifying any **.f90** files, re-compile **prog.main.f90**;
2. Among the **.f90** files, please replace **{your_working_dir}** with the path to your working directory, and replace **{your_fitness_function}** with the path to your defined fitness functions. Two examples are included in *fitness-function-examples*, one for mean absolute error (MAE) based on precipitation values, and one for normalized root mean square error (NRMSE) based on precipitation values. Replace the directory of observation files with your own data storage directory.


## Contact information:
* Dr. Thanh Nguyen-Xuan: nxthanhnx@gmail.com
* Ms. Zixuan Zhou: zzhoubh@connect.ust.hk
* Professor Eun-Soon Im: ceim@ust.hk
