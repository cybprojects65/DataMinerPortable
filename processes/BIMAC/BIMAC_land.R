library(raster)
library ( R2jags )
library ( coda )
library(plyr)
library(dplyr)
library(digest)

#ASC files definitions
if(!exists("punctual_data_file")){
  punctual_data_file = "C:/Users/Utente/Ricerca/Experiments/Web interfaces/test_data/temperature_argo.csv"
  smoothing<-1
  resolution<-0.5
  min_x_boundingbox=-0.25
  max_x_boundingbox=12
  min_y_boundingbox=34.75
  max_y_boundingbox=44
}

source("BIMAC_functions.R")

cat("\tpassed Boundaries (xmin,xmax,ymin,ymax):",min_x_boundingbox,",",max_x_boundingbox,",",min_y_boundingbox,",",max_y_boundingbox,"\n")

output<-bimac_onland(punctual_data_file,
                     moving_average_points=smoothing, 
                     fast_solving=T,
                     min_x_boundingbox=min_x_boundingbox,
                     max_x_boundingbox=max_x_boundingbox,
                     min_y_boundingbox=min_y_boundingbox,
                     max_y_boundingbox=max_y_boundingbox,
                     resolution=resolution)


