library(e1071)

if(!exists("input_column_names_codes")){
  input_column_names_codes<-c("longitude","latitude")
  #output_column_names_codes<-c("latitude","longitude")
  output_column_names_codes<-c("latitude")
  training_set_file<-"C:/Users/Utente/Ricerca/Experiments/Web interfaces/PseudoAbsenceGenerator/general_UI/tmp_data/presence_points.csv"
  test_set_file<-"C:/Users/Utente/Ricerca/Experiments/Web interfaces/PseudoAbsenceGenerator/general_UI/tmp_data/presence_points.csv"
  # rp - number of repetitions for the training - #20 gives more stability to the network and more independence of the initial training step
}
cat("INPUT PARAMETERS FOR THE SVM:\n")
cat("input_column_names_codes:",as.character(input_column_names_codes),"\n")
cat("output_column_names_codes:",as.character(output_column_names_codes),"\n")
cat("training_set_file:",as.character(training_set_file),"\n")
cat("test_set_file:",as.character(test_set_file),"\n")
cat("#######:\n")

#OUTPUT
ts_projected_file="training_set_with_projections.csv"
test_projected_file="test_set_with_projections.csv"
svm_out_file_name="svm.Rdata"
svm_out_file_logs="svm_logs.txt"


scale_minmax <- function(df, mins, maxs) {
  scaled <- as.data.frame(mapply(function(x, minv, maxv) {
    (x - minv) / (maxv - minv)
  }, df, mins, maxs, SIMPLIFY = FALSE))
  
  return(scaled)
}

# Reverse transformation
unscale_df <- function(scaled_df, original_min, original_max) {
  unscaled <- as.data.frame(mapply(function(x, minv, maxv) {
    x * (maxv - minv) + minv
  }, scaled_df, original_min, original_max, SIMPLIFY = FALSE))
  
  return(unscaled)
}


training_set<-read.csv(training_set_file)
training_set_input_raw<-as.data.frame(training_set[, c(input_column_names_codes), drop = FALSE])

input_min <- sapply(training_set_input_raw, min, na.rm = TRUE)
input_max <- sapply(training_set_input_raw, max, na.rm = TRUE)
training_set_input_scaled <- as.data.frame(scale_minmax(training_set_input_raw,input_min,input_max))
names(training_set_input_scaled)<-names(training_set_input_raw)

training_set_output_raw<-as.data.frame(training_set[, c(output_column_names_codes), drop=FALSE])
output_min <- sapply(training_set_output_raw, min, na.rm = TRUE)
output_max <- sapply(training_set_output_raw, max, na.rm = TRUE)
training_set_output_scaled <- as.data.frame(scale_minmax(training_set_output_raw,output_min,output_max))
names(training_set_output_scaled)<-names(training_set_output_raw)

cat("SVM training with multiple hidden layers\n")
training_set_features_only_ANN<-cbind(training_set_input_scaled,training_set_output_scaled)
new_output_columns<-paste0("proj_",names(training_set_output_scaled))

names(training_set_features_only_ANN)<-c(names(training_set_input_scaled),new_output_columns)
f <- as.formula(
  paste(
    paste(new_output_columns, collapse = " + "),
    "~",
    paste(input_column_names_codes, collapse = " + ")
  )
)

set.seed(20)
log_file <- file(svm_out_file_logs, open = "wt")
sinkit=F
if (sinkit){
  sink(log_file, type = "output")  # redirect standard output to the file
  sink(log_file, type = "message") # redirect messages too (warnings, messages)
}

#build up the SVM

#initialise features that will record the optimal scores
opt_cost<- "NA"
opt_gamma<- "NA"
opt_coef0 <- "NA"
opt_degree <- "NA"
opt_kernel <- ""

opt_accuracy<-0
opt_accuracy_self<-0
opt_threshold<-0

cat("SVM tuning: searching for the optimal configuration \n")

### Support Vector Machine Parameters ###
costlist =c(0.001,0.01,0.1,1,10,100)
kernellistx =c("linear","polynomial","radial","sigmoid")
# coef0list is used in "polynomial"and "sigmoid"
coef0list =c(0,1)
# gammalist is used in "polynomial","radial", "sigmoid"
gammalist =c(0.0001,0.001,0.01,0.1,1,10)
# degreelist is used in "polynomial"
degreelist =c(3,4) 
# nfold
cross95 =10
# It means "I will try to do not use data shrinking"	
do_the_shrinking =F

suppressWarnings({
  svm_tuned <- tune(
    svm, f, 
    data = training_set_features_only_ANN,
    ranges = list(
      kernel = kernellistx,   # typo fixed: should be kernellistx not kernellist
      cost = costlist, 
      gamma = gammalist, 
      coef0 = coef0list,
      degree = degreelist
    ), 
    scale = FALSE,
    tunecontrol = tune.control(sampling = "fix",fix = 0.8)
  )
})
print(svm_tuned)

best_model <- svm_tuned$best.model

ts_df<-training_set_features_only_ANN[,1:length(input_column_names_codes),drop=FALSE]

TS_pred <- as.matrix(predict(best_model, ts_df,probability = F))

errorTS<-mean((as.matrix(TS_pred) - as.matrix(training_set_output_scaled))^2)

cat("\n\nMean Squared Error on the training set:",errorTS,"\n")
cat("Training complete.\n")


if(sinkit){
  sink(type = "message")
  sink(type = "output")
  close(log_file)
  closeAllConnections()
}

unscaled_output_ts<-unscale_df(as.data.frame(TS_pred),output_min,output_max)
training_set_out<-cbind(training_set, unscaled_output_ts)
names(training_set_out)<-c(names(training_set),new_output_columns)
write.csv(x=training_set_out,file=ts_projected_file,row.names = F)

test_set<-read.csv(test_set_file)
test_set_features_only_ANN_raw <- test_set[, c(input_column_names_codes),drop=FALSE]
test_set_features_only_ANN_scaled<-as.data.frame(scale_minmax(test_set_features_only_ANN_raw, input_min,input_max))

test_pred_mat <- as.matrix(predict(best_model, test_set_features_only_ANN_scaled ,probability = F))

#save the projection on the test set
test_pred<-as.data.frame(as.matrix(test_pred_mat))
unscaled_output_test<-unscale_df(test_pred,output_min,output_max)

test_out<-cbind(test_set,unscaled_output_test)
names(test_out)<-c(names(test_set),new_output_columns)
write.csv(x=test_out,file=test_projected_file,row.names = F)

#save the neural net
save(input_min,input_max, output_min, output_max,svm_tuned, file=svm_out_file_name)   

