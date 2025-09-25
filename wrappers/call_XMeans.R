
doCall<-function(script_folder,params){
  
  cat(paste(params),"\n")
  
  training_set_file<-params[[1]][4]
  input_column_names_codes<-c(params[[2]])
  k_min<-as.numeric(params[[3]])
  k_max<-as.numeric(params[[4]])
  max_iterations<-as.numeric(params[[5]])

  # Save current working directory
  old_wd <- getwd()
  cat("setting folder to",script_folder,"\n")
  setwd(script_folder)

    result <- tryCatch(
  {
    cat("Executing the clustering script\n")
    source("X_means.R", local = TRUE)
    cat("End - Executing the script\n")
    c("input_data_with_clusters.csv","cluster_centroids_standardised.csv","cluster_centroids.csv","clustering.Rdata","clustering_logs.txt")
  },
  error = function(e) {
    cat("Error while running the model:", conditionMessage(e), "\n")
    showNotification("Error: the selection and the computation did not produce results", type = "message")
    # return empty list if failure
    c()
    }
  )

cat("check computation: ",file.exists("input_data_with_clusters.csv"),"\n")
if (!file.exists("input_data_with_clusters.csv"))
  result<-c()

cat("results retrieved\n")
setwd(old_wd)

expected_outputs<-result
return(expected_outputs)

}
