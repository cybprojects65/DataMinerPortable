library(mclust)

if(!exists("input_column_names_codes")){
  #input_column_names_codes<-c("longitude","latitude")
  input_column_names_codes<-c("longitude")
  training_set_file<-"C:/Users/Utente/Ricerca/Experiments/Web interfaces/PseudoAbsenceGenerator/general_UI/tmp_data/presence_points.csv"
  k_min<-2
  k_max<-3
  max_iterations<-100
}

cat("INPUT PARAMETERS FOR THE clustering:\n")
cat("input_column_names_codes:",as.character(input_column_names_codes),"\n")
cat("training_set_file:",as.character(training_set_file),"\n")
cat("k_min:",as.numeric(k_min),"\n")
cat("k_max:",as.numeric(k_max),"\n")
cat("max_iterations:",as.numeric(max_iterations),"\n")
cat("#######:\n")

#OUTPUT
ts_projected_file="input_data_with_clusters.csv"
cluster_centroids_scaled_file="cluster_centroids_standardised.csv"
cluster_centroids_file="cluster_centroids.csv"
model_out_file_name="clustering.Rdata"
model_out_file_logs="clustering_logs.txt"

zscore <- function(df) {
  means <- sapply(df, mean, na.rm = TRUE)
  sds   <- sapply(df, sd,   na.rm = TRUE)
  
  scaled <- as.data.frame(scale(df, center = means, scale = sds))
  
  
  return(list(
    scaled = scaled,  # standardized data
    means  = means,   # column means
    sds    = sds      # column standard deviations
  ))
}

# Destandardize (inverse transform)
un_zscore <- function(scaled_df, means, sds) {
  unscaled <- as.data.frame(mapply(function(x, m, s) {
    x * s + m
  }, scaled_df, means, sds, SIMPLIFY = FALSE))
  
  return(unscaled)
}

#read the training set
training_set<-read.csv(training_set_file)
#retrieve the input variables
training_set_input_raw<-as.data.frame(training_set[, c(input_column_names_codes), drop = FALSE])
training_set_input_scaling <- zscore(training_set_input_raw)
training_set_input_scaled<-training_set_input_scaling$scaled
#retrieve the input variables
input_means <- training_set_input_scaling$means
input_sds <- training_set_input_scaling$sds
names(training_set_input_scaled)<-names(training_set_input_raw)

# Study of centroids
cat(paste0("Study of centroids", "\n"))

#init logging
set.seed(20)
log_file <- file(model_out_file_logs, open = "wt")
sinkit=T
if (sinkit){
  sink(log_file, type = "output")  # redirect standard output to the file
  sink(log_file, type = "message") # redirect messages too (warnings, messages)
}

cat("####I'm analyzing ",k_min,"-",k_max,"centroids\n")

mc <- Mclust(as.matrix(training_set_input_scaled), G = k_min:k_max, control = emControl(itmax = max_iterations, tol = 1e-5))  # G = range of clusters

training_set$cluster <- mc$classification
if (!is.matrix(mc$parameters$mean) && is.vector(mc$parameters$mean)){
  centroids <- as.data.frame(matrix(mc$parameters$mean,ncol=1))
}else{
  centroids <- as.data.frame(t(mc$parameters$mean))
}
cat("####optimal centroids: ",nrow(centroids),"\n")

# Extract cluster means in standardized space
#centroids <- as.data.frame(t(mc$parameters$mean))
names(centroids) <- names(training_set_input_scaled)

centroids_descaled<-un_zscore(centroids,input_means,input_sds)

cat("clustering complete.\n")


#close logging
if(sinkit){
  sink(type = "message")
  sink(type = "output")
  close(log_file)
  closeAllConnections()
}

write.csv(x=training_set,file=ts_projected_file,row.names = F)
write.csv(x=centroids,file=cluster_centroids_scaled_file,row.names = F)
write.csv(x=centroids_descaled,file=cluster_centroids_file,row.names = F)

model_out_file_name="clustering.Rdata"

#save the neural net
save(input_means,input_sds,centroids,k_min,k_max,file=model_out_file_name)   

