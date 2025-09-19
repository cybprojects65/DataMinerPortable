library(raster)
library ( R2jags )
library ( coda )
library(plyr)
library(dplyr)
library(digest)

#ASC files definitions
if(!exists("punctual_data_file")){
  currents_u_file = "C:/Users/Utente/Ricerca/Experiments/Web interfaces/test_data/oceanic_currents_u_wm.asc"
  currents_v_file = "C:/Users/Utente/Ricerca/Experiments/Web interfaces/test_data/oceanic_currents_v_wm.asc"
  punctual_data_file = "C:/Users/Utente/Ricerca/Experiments/Web interfaces/test_data/temperature_argo.csv"
  analysis_depth<--1
  smoothing<-1
  sd_advection_equation<-0.1
}

source("BIMAC_functions.R")

output<-bimac_currents_depthboundaries(punctual_data_file,
                                       currents_u_file,
                                       currents_v_file,
                                       analysis_depth=-1,
                                       moving_average_points=1, 
                                       fast_solving=T, 
                                       sd_advection_equation=0.1
)

