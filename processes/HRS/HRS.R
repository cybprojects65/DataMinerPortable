cat("Machine address:\n")
print(system("hostname"))

softwaretype<-NA

#parameters of the process to invoke
false<-F
true<-T
softwaretype<-'Java'
softwareName<-'hrs.jar'
ClassToRun<-"org.cnr.datanalysis.ecomod.test.Main"
cat('Input Parameter:ClassToRun',ClassToRun,'\n')

if(!exists("InputHabitatTable")){
  InputHabitatTable<-"C:/Users/Utente/Ricerca/Experiments/Web interfaces/test_data/madagascar_features.csv"
  ReferenceHabitatTable<-"C:/Users/Utente/Ricerca/Experiments/Web interfaces/test_data/indonesia_features.csv"
  MaxBins<-"10"
  input_column_names_codes<-c("salinity","temperature")
}



first_line_A <- readLines(InputHabitatTable, n = 1)
has_header_A <- any(grepl("[A-Za-z]", strsplit(first_line_A, ",")[[1]]))
if (has_header_A){
  A<-read.csv(InputHabitatTable)
  A<-A[, c(input_column_names_codes), drop = FALSE]
  InputHabitatTable="A.csv"
  write.table(x=A,file = InputHabitatTable,sep=",",row.names = F,col.names = F)
}
first_line_B <- readLines(ReferenceHabitatTable, n = 1)
has_header_B <- any(grepl("[A-Za-z]", strsplit(first_line_B, ",")[[1]]))
if(has_header_B){
  B<-read.csv(ReferenceHabitatTable)
  B<-B[, c(input_column_names_codes), drop = FALSE]
  ReferenceHabitatTable="B.csv"
  write.table(x=B,file = ReferenceHabitatTable,sep=",",row.names = F,col.names = F)
}

cat('Input Parameter:InputHabitatTable',InputHabitatTable,'\n')

cat('Input Parameter:ReferenceHabitatTable',ReferenceHabitatTable,'\n')

cat('Input Parameter:MaxBins',MaxBins,'\n')
overallInput<-'ClassToRun	InputHabitatTable	ReferenceHabitatTable	MaxBins'
overallInputValues<-paste(ClassToRun,InputHabitatTable,ReferenceHabitatTable,MaxBins,sep='	')
overallInputTypes<-'System	File	File	Integer'



external_process_to_invoke<-'hrs.jar' #can be a python, fortran, linux-compiled, octave, java process
#prepare the command to invoke
command = paste(external_process_to_invoke,paste('"',ClassToRun,'"',sep=''),paste('"',InputHabitatTable,'"',sep=''),paste('"',ReferenceHabitatTable,'"',sep=''),paste('"',MaxBins,'"',sep=''),sep=" ")
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
OutputFile<-"hrs_summary.txt"
OutputFile2<-"logs.txt"
writeLines(text=logs,OutputFile2)
cat('Output Parameter:OutputFile',OutputFile,'\n')



#check if the output exists before exiting
fexists = file.exists(paste(OutputFile,sep=''))

cat("file OutputFile exists?",fexists,"\n")
if (!fexists){
	cat(fexists,"Error, the output OutputFile(hrs_summary.txt) does not exist!\n")
	stoptheprocess
}


