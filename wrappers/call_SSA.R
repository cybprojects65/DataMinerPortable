
doCall<-function(script_folder,params){
  
  cat(paste(params),"\n")
  
  inputtable<-params[[1]][4]
  timeColumn<-as.character(params[[2]])
  valueColum<-as.character(params[[3]])
  SSAPointsToForecast<-as.numeric(params[[4]])
  SSAEigenvaluesThreshold<-as.numeric(params[[5]])
  SSAAnalysisWindowSamples<-as.numeric(params[[6]])
  Adapt<-as.character(params[[7]])
  
  # Save current working directory
  old_wd <- getwd()
  cat("setting folder to",script_folder,"\n")
  setwd(script_folder)

    result <- tryCatch(
  {
    cat("Executing the clustering script\n")
    source("SSA.R", local = TRUE)
    cat("End - Executing the script\n")
    
    files <- list.files("your_folder", full.names = TRUE)
    # match anything that has characters before "gebco_30sec_8"
    Summary<-"summary.txt"
    GapfilledTimeSeriesImage<-"signal_u.png"
    GapFilledTimeSeriesCSV<-"signal_u.csv"
    ForecastImage<-"forecast.png"
    ForecastCSV<-"forecast.csv"
    EigenvaluesPercentagesImage<-"eigenvalues.png"
    EigenvaluesPercentagesCSV<-"eigenvalues.csv"
    TimeSeriesAsSignalImage<-"signal_samples_u.png"
    c(Summary,ForecastImage,ForecastCSV,EigenvaluesPercentagesImage,EigenvaluesPercentagesCSV,TimeSeriesAsSignalImage,GapfilledTimeSeriesImage,GapFilledTimeSeriesCSV)
  },
  error = function(e) {
    cat("Error while running the model:", conditionMessage(e), "\n")
    showNotification("Error: the selection and the computation did not produce results", type = "message")
    # return empty list if failure
    c()
    }
  )

checkfile<-"forecast.png"
cat("check computation: ",file.exists(checkfile),"\n")
if (!file.exists(checkfile))
  result<-c()

cat("results retrieved\n")
setwd(old_wd)

expected_outputs<<-result

}
