# RegCM5-MGA
These are codes for an optimization framework RegCM5-MGA that couples the fifth version of Regional Climate Model (RegCM5) with a microgenetic algorithm (MGA). It is specifically used for optimizing five Tiedtke scheme parameters over one selected domain and one (continuous) timeframe. For the parameter details, please refer to the paper.

## General Usage Instructions

After modifying any .f90 files, compile by running this command (*runga* can be replaced by other names that you desire):
ifort prog.main.f90 -o runga.exe

Among the files included, please replace **{your_working_dir}** with the path to your working directory
Among the files included, please replace **{your_fitness_function}** with the path to your defined fitness functions

Two examples are included in *fitness-function-examples*, one for mean absolute error (MAE) based on precipitation values, and one for normalized root mean square error (NRMSE) based on precipitation values.

The authors executed the programs based on GNU/Linux system, and the jobs are distributed to five mpihosts. Please ensure you have adapted the program to your system before running.

Contact information:
Dr.Thanh Nguyen-Xuan: nxthanhnx@gmail.com
Ms. Zixuan Zhou: zzhoubh@connect.ust.hk
Professor Eun-Soon Im: ceim@ust.hk
