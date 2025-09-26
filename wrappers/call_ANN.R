
doCall<-function(script_folder,params){
  
  cat(paste(params),"\n")
  
  training_set_file<-params[[1]][4]
  input_column_names_codes<-c(params[[2]])
  output_column_names_codes<-c(params[[3]])
  hiddens<-as.character(params[[4]])
  rp<-as.numeric(params[[5]])
  test_set_file<-params[[6]][4]

  # Save current working directory
  old_wd <- getwd()
  cat("setting folder to",script_folder,"\n")
  setwd(script_folder)

  cat("Executing the script\n")

  result <- tryCatch(
  {
    cat("Executing the script\n")
    source("FeedForwardANN.R", local = TRUE)
    cat("End - Executing the script\n")
    c("training_set_with_projections.csv","test_set_with_projections.csv","ann.Rdata")
  },
  error = function(e) {
    cat("Error while running FeedForwardANN.R:", conditionMessage(e), "\n")
    showNotification("Error: the selection and the computation did not produce results", type = "message")
    # return empty list if failure
    c()
    }
  )

cat("check computation: ",file.exists("training_set_with_projections.csv"),"\n")
if (!file.exists("training_set_with_projections.csv"))
  result<-c()

#source("PseudoAbsenceGenerator.R", local = TRUE)
cat("results retrieved\n")
setwd(old_wd)

expected_outputs<-result
return(expected_outputs)

}
