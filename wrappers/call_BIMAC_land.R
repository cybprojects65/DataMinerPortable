
doCall<-function(script_folder,params){
  
  cat(paste(params),"\n")
  
  punctual_data_file = params[[1]][4]
  min_x_boundingbox=as.numeric(params[[2]])
  max_x_boundingbox=as.numeric(params[[3]])
  min_y_boundingbox=as.numeric(params[[4]])
  max_y_boundingbox=as.numeric(params[[5]])
  resolution<-as.numeric(params[[6]])
  smoothing<-as.numeric(params[[7]])
  
  
  # Save current working directory
  old_wd <- getwd()
  cat("setting folder to",script_folder,"\n")
  setwd(script_folder)

    result <- tryCatch(
  {
    cat("Executing the clustering script\n")
    source("BIMAC_land.R", local = TRUE)
    cat("End - Executing the script\n")
    
    files <- list.files(script_folder, full.names = TRUE)
    # match anything that has characters before "gebco_30sec_8"
    to_delete <- files[grepl("^[^/]*_gebco_30sec_8.asc$", basename(files))]
    file.remove(to_delete)
    
    c("./output/BIMAC_interpolation.asc","./output/BIMAC_interpolation_sd.asc","./output/BIMAC_IDW_prior.asc")
  },
  error = function(e) {
    cat("Error while running the model:", conditionMessage(e), "\n")
    showNotification("Error: the selection and the computation did not produce results", type = "message")
    # return empty list if failure
    c()
    }
  )

checkfile<-"./output/BIMAC_IDW_prior.asc"
cat("check computation: ",file.exists(checkfile),"\n")
if (!file.exists(checkfile))
  result<-c()

cat("results retrieved\n")
setwd(old_wd)

expected_outputs<-result
return(expected_outputs)

}
