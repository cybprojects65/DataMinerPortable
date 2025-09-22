cat("Machine address:\n")
print(system("hostname"))

softwaretype<-NA

#parameters of the process to invoke
false<-F
true<-T
softwaretype<-'Java'
softwareName<-'ssa.jar'
ClassToRun<-"it.cnr.timeseries.analysis.test.Main"
cat('Input Parameter:ClassToRun',ClassToRun,'\n')

if(!exists("inputtable")){
  inputtable<-"C:/Users/Utente/Ricerca/Experiments/Web interfaces/test_data/sample_timeseries.csv"
  timeColumn<-"year"  
  valueColum<-"biomindex"  
  SSAPointsToForecast<-"1"
  SSAEigenvaluesThreshold<-"0.01"
  SSAAnalysisWindowSamples<-"6"
  Adapt<-"false"
}

cat('Input Parameter:inputtable',inputtable,'\n')
cat('Input Parameter:timeColumn',timeColumn,'\n')
cat('Input Parameter:valueColum',valueColum,'\n')
cat('Input Parameter:SSAPointsToForecast',SSAPointsToForecast,'\n')
cat('Input Parameter:SSAEigenvaluesThreshold',SSAEigenvaluesThreshold,'\n')
cat('Input Parameter:SSAAnalysisWindowSamples',SSAAnalysisWindowSamples,'\n')
cat('Input Parameter:Adapt',Adapt,'\n')

overallInput<-'ClassToRun	inputtable	timeColumn	valueColum	SSAPointsToForecast	SSAEigenvaluesThreshold	SSAAnalysisWindowSamples	Adapt'
overallInputValues<-paste(ClassToRun,inputtable,timeColumn,valueColum,SSAPointsToForecast,SSAEigenvaluesThreshold,SSAAnalysisWindowSamples,Adapt,sep='	')
overallInputTypes<-'System	File	String	String	Integer	Double	Integer	Boolean'



external_process_to_invoke<-'ssa.jar' #can be a python, fortran, linux-compiled, octave, java process
#prepare the command to invoke
command = paste(external_process_to_invoke,paste('"',ClassToRun,'"',sep=''),paste('"',inputtable,'"',sep=''),paste('"',timeColumn,'"',sep=''),paste('"',valueColum,'"',sep=''),paste('"',SSAPointsToForecast,'"',sep=''),paste('"',SSAEigenvaluesThreshold,'"',sep=''),paste('"',SSAAnalysisWindowSamples,'"',sep=''),paste('"',Adapt,'"',sep=''),sep=" ")
cat('Command:',command,'\n')



cat("Adding rights to the files\n")
logs<-system("chmod 777 *", intern = TRUE)
cat("computation started\n")
cat("software type:",softwaretype,"\n")

#global variables
gcube_username<-ifelse(exists("gcube_username"),gcube_username,"")
gcube_context<-ifelse(exists("gcube_context"),gcube_context,"")
gcube_token<-ifelse(exists("gcube_token"),gcube_token,"")
host<-system("hostname")

globalvariable = (c("gcube_username", "gcube_context", "gcube_token","hostname"))
globalvalue = (c(gcube_username, gcube_context, gcube_token,host))
processdf = data.frame(globalvariable, globalvalue)
cat("Global variables:\n")
print(processdf)
write.csv(file="globalvariables.csv",processdf,row.names = F)

#run the process
if (is.na(softwaretype)){
}else if (softwaretype=='Windows-compiled'){
	cat("Windows computation\n")
	command<-paste('mono ./',command,sep='')
}else if (softwaretype=='Linux-compiled'){
	cat("Linux computation\n")
	command<-paste('./',command,sep='')
}else if (softwaretype=='Pre-Installed'){
	cat("Pre-installed computation\n")
	cat("Cleaning dos lines\n")
	softwareNameTemp<-paste(softwareName,".1",sep="")
	cleanDosLines<-paste("sed -e 's/\x0D$//' ",softwareName," > ", softwareNameTemp,sep="")
	cat("Cleaning command step 1:",cleanDosLines,"\n")
	system(cleanDosLines, intern = TRUE)
	cleanDosLines<-paste("cp ",softwareNameTemp," ", softwareName,sep="")
	cat("Cleaning command step 2:",cleanDosLines,"\n")
	system(cleanDosLines, intern = TRUE)
	command<-paste('./',command,sep='')
}else if (softwaretype=='R-blackbox'){
	cat("R computation\n")
	command<-paste('Rscript --vanilla ',command,sep='')
}else if (softwaretype=='Python'){
	cat("Python computation\n")
	#command<-paste('python ',command,sep='')
	command<-paste('python ',command,sep='')
}else if (softwaretype=='Python3.6'){
	cat("Python 3.6 computation\n")
	command<-paste('python3.6 ',command,sep='')	
}else if (softwaretype=='Java'){
	cat("Java computation\n")
	command<-paste('java -cp ./',command,sep='')
}else if (softwaretype=='Octave'){
	cat("Octave computation\n")
	command<-paste('octave ./',command,sep='')
}else if (softwaretype=='Knime-Workflow'){
  cat("Knime 3 computation\n")
  workflow<-"workflow.knwf"
  if (file.exists(paste(softwareName,'.zip',sep='')))
    file.rename(paste(softwareName,'.zip',sep=''),softwareName)
  #EXAMPLE: knime -reset -consoleLog -nosplash -application org.knime.product.KNIME_BATCH_APPLICATION 
  #-workflowFile=SimpleSample.knwf -destFile=out.zip -workflow.variable=test,"besttest",String
  oI<-strsplit(overallInput,"\t")
  oIv<-strsplit(overallInputValues,"\t")
  oIt<-strsplit(overallInputTypes,"\t")

  knimevariables<-""
  for (i in 1:length(oI[[1]])){
		oinp<-oI[[1]][i]
		oinpv<-oIv[[1]][i]
		oinpt<-oIt[[1]][i]
		if (tolower(oinpt) == 'integer')
			oinpt<-"int"
		else if (tolower(oinpt) == 'double')
			oinpt<-"double"
		else {
			oinpv<-gsub(' ', '_', oinpv)
			oinpv<-gsub(',', ';', oinpv)
			oinpt<-"String"
		}
		
		knimevar = paste(oinp,",",'"',oinpv,'"',",",oinpt,sep="")
		
		knimevariables<-paste(knimevariables,' -workflow.variable=',knimevar,sep="")
		
  }
  
  if (file.exists("preference.epf")){
	command<-paste('/usr/local/bin/knime -reset -consoleLog -nosplash -preferences=preference.epf -application org.knime.product.KNIME_BATCH_APPLICATION -workflowFile=',softwareName,' -destFile=',workflow,knimevariables, sep='')
  } else {
  	command<-paste('/usr/local/bin/knime -reset -consoleLog -nosplash -application org.knime.product.KNIME_BATCH_APPLICATION -workflowFile=',softwareName,' -destFile=',workflow,knimevariables, sep='')
  }
}else if (softwaretype=='Knime-Workflow4.1'){
	cat("Knime 4 computation\n")
	workflow<-"workflow.knwf"
	if (file.exists(paste(softwareName,'.zip',sep='')))
		file.rename(paste(softwareName,'.zip',sep=''),softwareName)
	#EXAMPLE: knime -reset -consoleLog -nosplash -application org.knime.product.KNIME_BATCH_APPLICATION 
	#-workflowFile=SimpleSample.knwf -destFile=out.zip -workflow.variable=test,"besttest",String
	oI<-strsplit(overallInput,"\t")
	oIv<-strsplit(overallInputValues,"\t")
	oIt<-strsplit(overallInputTypes,"\t")
	
	knimevariables<-""
	for (i in 1:length(oI[[1]])){
		oinp<-oI[[1]][i]
		oinpv<-oIv[[1]][i]
		oinpt<-oIt[[1]][i]
		if (tolower(oinpt) == 'integer')
			oinpt<-"int"
		else if (tolower(oinpt) == 'double')
			oinpt<-"double"
		else {
			oinpv<-gsub(' ', '_', oinpv)
			oinpv<-gsub(',', ';', oinpv)
			oinpt<-"String"
		}
		
		knimevar = paste(oinp,",",'"',oinpv,'"',",",oinpt,sep="")
		
		knimevariables<-paste(knimevariables,' -workflow.variable=',knimevar,sep="")
		
	}
	
	if (file.exists("preference.epf")){
		command<-paste('/usr/local/bin/knime4.1 -reset -consoleLog -nosplash -preferences=preference.epf -application org.knime.product.KNIME_BATCH_APPLICATION -workflowFile=',softwareName,' -destFile=',workflow,knimevariables, sep='')
	}else {
		command<-paste('/usr/local/bin/knime4.1 -reset -consoleLog -nosplash -application org.knime.product.KNIME_BATCH_APPLICATION -workflowFile=',softwareName,' -destFile=',workflow,knimevariables, sep='')	
	}	
	
}
	
cat(command,"\n")
logs<-system(command, intern = TRUE)
cat("computation finished\n")
cat("Logs of the computation\n")
print(logs)
cat("\n")


#prepare the final output file
outputfolder <- getwd() #current working folder (a temporary folder is created at each run and set as working directory)
Summary<-"summary.txt"
cat('Output Parameter:Summary',Summary,'\n')
GapfilledTimeSeriesImage<-"signal_u.png"
cat('Output Parameter:GapfilledTimeSeriesImage',GapfilledTimeSeriesImage,'\n')
GapFilledTimeSeriesCSV<-"signal_u.csv"
cat('Output Parameter:GapFilledTimeSeriesCSV',GapFilledTimeSeriesCSV,'\n')
ForecastImage<-"forecast.png"
cat('Output Parameter:ForecastImage',ForecastImage,'\n')
ForecastCSV<-"forecast.csv"
cat('Output Parameter:ForecastCSV',ForecastCSV,'\n')
EigenvaluesPercentagesImage<-"eigenvalues.png"
cat('Output Parameter:EigenvaluesPercentagesImage',EigenvaluesPercentagesImage,'\n')
EigenvaluesPercentagesCSV<-"eigenvalues.csv"
cat('Output Parameter:EigenvaluesPercentagesCSV',EigenvaluesPercentagesCSV,'\n')
TimeSeriesAsSignalImage<-"signal_samples_u.png"
cat('Output Parameter:TimeSeriesAsSignalImage',TimeSeriesAsSignalImage,'\n')



#check if the output exists before exiting
fexists = file.exists(paste(Summary,sep=''))
cat("file Summary exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output Summary(summary.txt) does not exist!\n")
	stoptheprocess
}

fexists = file.exists(paste(GapfilledTimeSeriesImage,sep=''))
cat("file GapfilledTimeSeriesImage exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output GapfilledTimeSeriesImage(signal_u.png) does not exist!\n")
	stoptheprocess
}

fexists = file.exists(paste(GapFilledTimeSeriesCSV,sep=''))
cat("file GapFilledTimeSeriesCSV exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output GapFilledTimeSeriesCSV(signal_u.csv) does not exist!\n")
	stoptheprocess
}

fexists = file.exists(paste(ForecastImage,sep=''))
cat("file ForecastImage exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output ForecastImage(forecast.png) does not exist!\n")
	stoptheprocess
}

fexists = file.exists(paste(ForecastCSV,sep=''))
cat("file ForecastCSV exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output ForecastCSV(forecast.csv) does not exist!\n")
	stoptheprocess
}

fexists = file.exists(paste(EigenvaluesPercentagesImage,sep=''))
cat("file EigenvaluesPercentagesImage exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output EigenvaluesPercentagesImage(eigenvalues.png) does not exist!\n")
	stoptheprocess
}

fexists = file.exists(paste(EigenvaluesPercentagesCSV,sep=''))
cat("file EigenvaluesPercentagesCSV exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output EigenvaluesPercentagesCSV(eigenvalues.csv) does not exist!\n")
	stoptheprocess
}

fexists = file.exists(paste(TimeSeriesAsSignalImage,sep=''))
cat("file TimeSeriesAsSignalImage exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output TimeSeriesAsSignalImage(signal_samples_u.png) does not exist!\n")
	stoptheprocess
}


