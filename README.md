# RegCM5-MGA
Code for an optimization framework that couples the fifth version of Regional Climate Model (RegCM5) with a microgenetic algorithm (MGA). It is specifically used for optimizing five Tiedtke scheme parameters over one selected domain and one (continuous) timeframe.

After modifying any .f90 files, you need to compile, run this command:
ifort prog.main.f90 -o runga.exe

Among the files included, please replace {your_working_dir} with the path to your working directory
Among the files included, please replace {your_fitness_function} with the path to your defined fitness functions

Two examples are included here, one for mean absolute error (MAE) based on precipitation values, and one for normalized root mean square error (NRMSE) based on precipitation values.

The authors executed the programs based on GNU/Linux system, and the work distributed to five mpihosts. Ensure you have adapted the program to your system before running.

For any additional questions, please contact the corresponding author Professor Eun-Soon Im: ceim@ust.hk
