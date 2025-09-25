
doCall<-function(script_folder,params){
  
  cat(paste(params),"\n")
  
  InputHabitatTable<-params[[1]][4]
  input_column_names_codes<-c(params[[2]])
  MaxBins<-as.numeric(params[[3]])
  ReferenceHabitatTable<-params[[4]][4]
  
  # Save current working directory
  old_wd <- getwd()
  cat("setting folder to",script_folder,"\n")
  setwd(script_folder)

    result <- tryCatch(
  {
    cat("Executing the clustering script\n")
    source("HRS.R", local = TRUE)
    cat("End - Executing the script\n")
    c("hrs_summary.txt","logs.txt")
  },
  error = function(e) {
    cat("Error while running the model:", conditionMessage(e), "\n")
    showNotification("Error: the selection and the computation did not produce results", type = "message")
    # return empty list if failure
    c()
    }
  )

cat("check computation: ",file.exists("hrs_summary.txt"),"\n")
if (!file.exists("hrs_summary.txt"))
  result<-c()

cat("results retrieved\n")
setwd(old_wd)

expected_outputs<-result
return(expected_outputs)
}
