library(neuralnet)

if(!exists("input_column_names_codes")){
  input_column_names_codes<-c("longitude","latitude")
  output_column_names_codes<-c("latitude","longitude")
  #output_column_names_codes<-c("latitude")
  hiddens<-"0"
  training_set_file<-"C:/Users/Utente/Ricerca/Experiments/Web interfaces/PseudoAbsenceGenerator/general_UI/tmp_data/presence_points.csv"
  test_set_file<-"C:/Users/Utente/Ricerca/Experiments/Web interfaces/PseudoAbsenceGenerator/general_UI/tmp_data/presence_points.csv"
  # rp - number of repetitions for the training - #20 gives more stability to the network and more independence of the initial training step
  rp =10
}
cat("INPUT PARAMETERS FOR THE ANN:\n")
cat("input_column_names_codes:",as.character(input_column_names_codes),"\n")
cat("output_column_names_codes:",as.character(output_column_names_codes),"\n")
cat("hiddens:",as.character(hiddens),"\n")
cat("training_set_file:",as.character(training_set_file),"\n")
cat("test_set_file:",as.character(test_set_file),"\n")
cat("rp:",as.character(rp),"\n")
cat("#######:\n")

#OUTPUT
ts_projected_file="training_set_with_projections.csv"
test_projected_file="test_set_with_projections.csv"
neural_out_file_name="ann.Rdata"
neural_out_file_logs="ann_logs.txt"



# thld - threshold for minimum decrease in overall error, default 0.01 = 1%
thld =0.001
# stp - the maximum steps for the training of the neural network, default 1e+05
stp =1e+06
# alg - possible: backprop, rprop+, rprop-, sag, or slr
alg ="rprop-"
# act.fct - possible: "logistic" or "tanh"; linear.output must be 
act.fct ="logistic"
#nfolds
nfold =1

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

cat("ANN training with multiple hidden layers\n")
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
hidden <- as.numeric(unlist(strsplit(hiddens, "\\|")))

cat("Training with hidden neurons =",paste(hidden),"\n")

log_file <- file(neural_out_file_logs, open = "wt")
sinkit=F
if (sinkit){
  sink(log_file, type = "output")  # redirect standard output to the file
  sink(log_file, type = "message") # redirect messages too (warnings, messages)
}

nn <- neuralnet(f,
                data = training_set_features_only_ANN,
                hidden = hidden, 
                threshold = thld, 
                stepmax = stp, 
                rep = rp, 
                act.fct = act.fct, 
                linear.output = FALSE, lifesign = "minimal", algorithm = alg) 

# Compute predictions on the training data
  if (length(input_column_names_codes)>1){
    pr.nn1<- compute(nn, training_set_features_only_ANN[,1:length(input_column_names_codes)])
    }else{
    pr.nn1<- compute(nn, training_set_features_only_ANN)
  }

TS_pred<- pr.nn1$net.result
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
#save the projection on the training set
training_set_out<-cbind(training_set, unscaled_output_ts)
names(training_set_out)<-c(names(training_set),new_output_columns)
write.csv(x=training_set_out,file=ts_projected_file,row.names = F)

test_set<-read.csv(test_set_file)
test_set_features_only_ANN_raw <- test_set[, c(input_column_names_codes),drop=FALSE]
test_set_features_only_ANN_scaled<-as.data.frame(scale_minmax(test_set_features_only_ANN_raw, input_min,input_max))

if (length(input_column_names_codes)>1){
  pr.nn_test<- compute(nn, test_set_features_only_ANN_scaled[,1:length(input_column_names_codes)])
}else{
  pr.nn_test<- compute(nn, test_set_features_only_ANN_scaled)
}

#save the projection on the test set
test_pred<-as.data.frame(as.matrix(pr.nn_test$net.result))
unscaled_output_test<-unscale_df(test_pred,output_min,output_max)

test_out<-cbind(test_set,unscaled_output_test)
names(test_out)<-c(names(test_set),new_output_columns)
write.csv(x=test_out,file=test_projected_file,row.names = F)



#save the neural net
save(input_min,input_max, output_min, output_max,nn, file=neural_out_file_name)   
   
   