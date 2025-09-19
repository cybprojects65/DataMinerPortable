
doCall<-function(script_folder,params){
  
cat(paste(params),"\n")   # for debugging
  species<-params[[1]]
  resolution<-as.numeric(params[[2]])
  yearStart<-as.numeric(params[[3]])
  yearEnd<-as.numeric(params[[4]])
  presence_only<-as.logical(params[[5]])
  boundingbox<-params[[6]]

  # Save current working directory
  old_wd <- getwd()
  cat("setting folder to",script_folder,"\n")
  setwd(script_folder)

  cat("Executing the script\n")

  result <- tryCatch(
  {
    cat("Executing the script\n")
    source("PseudoAbsenceGenerator.R", local = TRUE)
    cat("End - Executing the script\n")
    c("presence_absence_map.png","occurrence_records.csv","presence_points.csv","absence_points.csv")
  },
  error = function(e) {
    cat("Error while running PseudoAbsenceGenerator.R:", conditionMessage(e), "\n")
    showNotification("Error: the selection and the computation did not produce results", type = "message")
    # return empty list if failure
    c()
    }
  )

cat("check computation: ",file.exists("presence_absence_map.png"),"\n")
if (!file.exists("presence_absence_map.png"))
  result<-c()

#source("PseudoAbsenceGenerator.R", local = TRUE)
cat("results retrieved\n")
setwd(old_wd)

expected_outputs<<-result

}
