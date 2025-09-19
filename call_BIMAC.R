
doCall<-function(script_folder,params){
  
  cat(paste(params),"\n")
  
  punctual_data_file = params[[1]][4]
  currents_u_file = params[[2]][4]
  currents_v_file = params[[3]][4]
  analysis_depth<-as.numeric(params[[4]])
  smoothing<-as.numeric(params[[5]])
  sd_advection_equation<-as.numeric(params[[6]])
  
  # Save current working directory
  old_wd <- getwd()
  cat("setting folder to",script_folder,"\n")
  setwd(script_folder)

    result <- tryCatch(
  {
    cat("Executing the clustering script\n")
    source("BIMAC.R", local = TRUE)
    cat("End - Executing the script\n")
    
    files <- list.files("your_folder", full.names = TRUE)
    # match anything that has characters before "gebco_30sec_8"
    to_delete <- files[grepl("^[^/]*_gebco_30sec_8$", basename(files))]
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

expected_outputs<<-result

}
