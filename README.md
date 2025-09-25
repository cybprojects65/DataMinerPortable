# DataMiner Portable

A portable version of the DataMiner cloud computing platform described in

Coro, G., Panichi, G., Scarponi, P., & Pagano, P. (2017). Cloud computing in a distributed e‐infrastructure using the web processing service standard. *Concurrency and Computation: Practice and Experience*, *29*(18), e4219.

## HOW TO RUN DATAMINER PORTABLE

1.  Activate Docker on the PC.

2.  Execute:

    docker run -it --rm -p 3838:3838 gianpaolocoro/dataminer_portable:latest

## HOW TO ADD A NEW METHOD

1.  Create a folder under “processes” for your method (e.g., BIMAC)
2.  Create a wrapper for this method under “wrappers”.

The wrapper should implement the following function:

```         
doCall<-function(script_folder,params){}
```

with script_folder being the temporary folder in which your process will be copied params is a list that contains the input inserted through the interface by position, e.g.,

```         
  punctual_data_file = params[[1]][4]
  currents_u_file = params[[2]][4]
  currents_v_file = params[[3]][4]
  analysis_depth<-as.numeric(params[[4]])
  smoothing<-as.numeric(params[[5]])
  sd_advection_equation<-as.numeric(params[[6]])
```

The function should return either the list of output files local to the script folder, e.g.,

```         
c("./output/BIMAC_interpolation.asc","./output/BIMAC_interpolation_sd.asc","./output/BIMAC_IDW_prior.asc")
```

or the empty vector c() should the method crash or produce an empty output.

The doCall function should internally call the method, assuming that it is available in the script_folder.

3.  Build a configuration file under “configurations” whose name that begins with “config\_”. This file should contain at least the following properties:

process=<process wrapper name without the path> e.g., call_BIMAC_land.R

```         
process_folder=<the process folder created under processes> e.g., BIMAC

header=<the full name of the algorithm> e.g., BIMAC Interpolator without constrains

description=<long method description> e.g., An implementation of the BIMAC Interpolator by Coro 2023
```

Input variables can be added, which will correspond to options, inputs, buttons, etc. on the interface. The interface will create them in the same order as reported in the configuration file. Note that each input has a sequential number to distinguish it from the others.

*Input types:*

1.  numeric\_<N> (e.g., numeric_1): a numeric input, whose digit accuracy will be guessed from the input value

Content: "<description>","<default value>"

Example: numeric_1="Minimum longitude of the bounding box of the projection","-0.25"

2.  file_input\_<N>: an input file

Content: "<description>"

Example: file_input_1="Upload training set CSV file"

3.  column_selection\_<N>: a multiple selection of the columns in the file (if this is a CVS file)

Content:<reference file input variable>,"<description>"

Example: column_selection_1=file_input_1,"Input column(s)"

4.  single_column_selection\_<N>: a single selection among the columns in the file (if this is a CVS file)

Content:<reference file input variable>,"<description>"

Example: single_column_selection_1=file_input_1,"Column containing time information."

5.  text_area\_<N>: a textual input, useful for structured inputs that can be provided as strings

Content: "<description>","<default value>""

Example: text_area_1="Hidden neurons per layer (separated with pipe)","2\|2"

6.  bounding_box\_<N>: a global map that allows the user to trace a bounding box that is reported in WKT format.

Content: "<description>","<default value>"

Example: bounding_box_1="Bounding box",""

At the start of the application, this will read all configuration files to automatically build the list of algorithms and then an interface for each algorithm.

## HOW TO UPDATE DATAMINER PORTABLE

After adding a new method, update the latest docker version: 1. Go to the parent folder of DataMinerPortable 2. Create a Dockerfile with the following instructions:

```         
FROM gianpaolocoro/dataminer_portable:latest
RUN rm -rf /srv/shiny-server/*
RUN rm -rf /srv/shiny-server/dataminer/
COPY DataMinerPortable/app.R ./dataminer/
COPY DataMinerPortable/auxfunctions.R ./dataminer/
RUN mkdir -p ./dataminer/www
COPY DataMinerPortable/www/ ./dataminer/www/
RUN mkdir -p ./dataminer/wrappers
COPY DataMinerPortable/wrappers/ ./dataminer/wrappers/
RUN mkdir -p ./dataminer/processes
COPY DataMinerPortable/processes/ ./dataminer/processes/
RUN mkdir -p ./dataminer/configurations
COPY DataMinerPortable/configurations/ ./dataminer/configurations/
CMD ["R", "-e", "shiny::runApp('dataminer', host='0.0.0.0', port=3838)"]
```

3.  Execute the update:

```         
    docker build -t gianpaolocoro/dataminer_portable .
```

4.  Update the docker hub:

```         
    docker push gianpaolocoro/dataminer_portable:latest
```
