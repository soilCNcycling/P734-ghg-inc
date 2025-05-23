# P734-ghg-inc
R scripts for creating event files and processing gas flux data from P734 incubation system. 

## How to use
734inc_eventfile.r R script is used to generate "event files" for the incubation. This sets things such as the number of chambers that are measured, how long they are measured for, and the duration each chamber is flushed. 

"Flux_calculations.r" calculates CO2 and N2O flux from the raw ppm data output by the Aeris. Data files from the incubation systems are read from the "Aeris_data" folder. The calculated fluxes are compiled into a .csv file with a flux per chamber per datetime taken from the Aeris data file names. Typically, each raw data file includes one flux measurement per chamber. There may be multiple data files per day if the measurement sequence takes <24 h to complete.
