
decimal_places <- function(x) {
  # Convert number to character
  x_char <- format(x, scientific = FALSE, trim = TRUE)
  
  # Split at decimal point
  parts <- strsplit(x_char, ".", fixed = TRUE)[[1]]
  
  # If there’s a fractional part, count its length
  if (length(parts) == 2) {
    nchar(parts[2])
  } else {
    0  # no decimal part
  }
}

read_properties <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(paste("Properties file not found:", filepath))
  }
  lines <- readLines(filepath, warn = FALSE)
  # Remove comments and blank lines
  lines <- lines[!grepl("^\\s*#", lines)]
  lines <- lines[nzchar(trimws(lines))]
  
  props <- strsplit(lines, "=")
  props <- lapply(props, function(x) {
    key <- trimws(x[1])
    value <- trimws(x[2])
    value
  })
  names(props) <- sapply(strsplit(lines, "="), function(x) trimws(x[1]))
  
  return(props)
}

build_method_list<-function(applications,app_descriptions){
  appidx<-1
  button_list<-list(useShinyjs())
  eidx<-1
  for (i in 1:length(applications)){
    button_list[[eidx]]<-tags$em(paste0(substr(app_descriptions[i], 1, 34), ".."))
    eidx<-eidx+1
    button_list[[eidx]]<-actionButton(inputId = paste0("method_",i), label = applications[i],class = "btn btn-primary")
    eidx<-eidx+1
    appidx<-appidx+1
  }
  
  return(button_list)
}

build_input_list<-function(applications,selected_id,config_paths){
  if (selected_id == "RESET")
    return(list())
  
  methodidx <- as.numeric(sub("method_", "", selected_id))
  configfile<-paste0("config_",applications[methodidx],".properties")
  configfilepath<-file.path(config_paths,configfile)
  cat("reading configuration for algorithm ",applications[methodidx],"from",configfilepath,"\n")
  properties <- read_properties(configfilepath)
  header<- properties[["header"]]
  description<- properties[["description"]]
  process_to_call<- properties[["process"]]
  process_folder<- properties[["process_folder"]]
  
  cb_list<-list(useShinyjs())
  all_inputs<-c()
  needs_map<-F
  bounding_box_input_var<-""
  input_file_vars<-c()
  input_file_vars_uploaded<-c()
  column_vars<-c()
  column_vars_references<-c()
  single_column_vars<-c()
  single_column_vars_references<-c()
  
  for (k in 1:length(properties)){
    key <- names(properties)[k]
    v <- properties[[k]]
    if (grepl("^text_area_*", key, perl = TRUE)){
      parts <- strsplit(v, ",")[[1]]
      # Remove quotes
      parts <- gsub('^"|"$', '', parts)
      lab<-parts[1]
      val<-parts[2]
      i<-textInput(
        inputId = key,
        label = lab,
        value = val,
        width = "50%"
      )
      all_inputs<-c(all_inputs,key)
      cb_list[[length(cb_list) + 1]] <- i
      
    }else if (grepl("^numeric_*", key, perl = TRUE)){
      parts <- strsplit(v, ",")[[1]]
      # Remove quotes
      parts <- gsub('^"|"$', '', parts)
      lab<-parts[1]
      val<-parts[2]
      i<-shinyWidgets::autonumericInput(
        inputId = key, 
        label = lab, 
        value = val, 
        currencySymbolPlacement = "p",
        decimalPlaces = decimal_places(as.numeric(val)),
        digitGroupSeparator = ",",
        decimalCharacter = ".",
        width = "50%",
      )
      
      all_inputs<-c(all_inputs,key)
      cb_list[[length(cb_list) + 1]] <- i
    } else if (grepl("^select_input_*", key, perl = TRUE)){
      parts <- strsplit(v, ",")[[1]]
      # Remove quotes
      parts <- gsub('^"|"$', '', parts)
      lab<-parts[1]
      def<-parts[2]
      val<-parts[3]
      choices <- strsplit(val, "\\|")[[1]]
      li<-c()
      for (cc in choices){
        li<-c(li,cc)
        cat(cc,"\n")
      }
      
      i<-selectInput(
        inputId = key,
        label = lab,
        choices = as.list(li),
        selected = def,
        width = "50%",
      )
      all_inputs<-c(all_inputs,key)
      cb_list[[length(cb_list) + 1]] <- i
    }  else if (grepl("^bounding_box_*", key, perl = TRUE)){
      parts <- strsplit(v, ",")[[1]]
      # Remove quotes
      parts <- gsub('^"|"$', '', parts)
      lab<-parts[1]
      def<-parts[2]
      val<-parts[3]
      choices <- strsplit(val, "\\|")[[1]]
      li<-c()
      for (cc in choices){
        li<-c(li,cc)
        cat(cc,"\n")
      }
      
      j <- leafletOutput(outputId = paste0(key, "_map"), height = "400px")
      
      i<-textInput(
        inputId = key,
        label = lab,
        value = val,
        width = "100%"
      )
      bounding_box_input_var<-key
      needs_map<-T
      #all_inputs<-c(all_inputs,paste0(key, "_map"),key)
      all_inputs<-c(all_inputs,paste0(key, "_map"),key)
      cb_list[[length(cb_list) + 1]] <- j
      cb_list[[length(cb_list) + 1]] <- i
    } else if (grepl("^file_input*", key, perl = TRUE)){
      parts <- v
      # Remove quotes
      parts <- gsub('^"|"$', '', parts)
      lab<-parts
      
      i<-fileInput(inputId = key,
                   label = lab,
                   multiple = FALSE,
                   accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv", ".txt", ".json", ".zip", ".bin", ".dat", ".mat",".asc",".jpg",".png",".tiff",".tif"
                   ))
      
      input_file_vars<-c(input_file_vars,key)
      input_file_vars_uploaded<-c(input_file_vars_uploaded,NA)
      all_inputs<-c(all_inputs,key)
      cb_list[[length(cb_list) + 1]] <- i
      
    }else if (grepl("^column_selection_*", key, perl = TRUE)){
      parts <- strsplit(v, ",")[[1]]
      parts <- gsub('^"|"$', '', parts)
      reference<-parts[1]
      lab<-parts[2]
      
      i<-checkboxGroupInput(
        inputId = key,
        label = lab,
        choices = list()
      )
      
      column_vars<-c(column_vars,key)
      column_vars_references<-c(column_vars_references,reference)
      
      all_inputs<-c(all_inputs,key)
      cb_list[[length(cb_list) + 1]] <- i
    }else if (grepl("^single_column_selection_*", key, perl = TRUE)){
      parts <- strsplit(v, ",")[[1]]
      parts <- gsub('^"|"$', '', parts)
      reference<-parts[1]
      lab<-parts[2]
      
      i <- radioButtons(
        inputId = key,
        label = lab,
        choices = list(),
        selected = character(0) # start with none selected
      )
      
      
      single_column_vars<-c(single_column_vars,key)
      single_column_vars_references<-c(single_column_vars_references,reference)
      
      all_inputs<-c(all_inputs,key)
      cb_list[[length(cb_list) + 1]] <- i
    }
  }

  return(list(
    header=header,
    description=description,
    process_to_call=process_to_call,
    process_folder=process_folder,
    cb_list=cb_list,
         all_inputs=all_inputs,
         needs_map=needs_map,
         bounding_box_input_var=bounding_box_input_var,
         input_file_vars=input_file_vars,
         input_file_vars_uploaded=input_file_vars_uploaded,
         column_vars=column_vars,
         column_vars_references=column_vars_references,
    single_column_vars=single_column_vars,
    single_column_vars_references=single_column_vars_references
    ))
}

duplicate_folder_with_uuid <- function(src_path,src_folder) {
  cat("poppy\n")
  print(src_path)
  cat("poppys\n")
  print(src_folder)
  source_actual<-file.path(src_path, basename(src_folder))
  if (!dir.exists(source_actual)) stop("Source folder does not exist")
  
  # Generate UUID prefix
  id <- UUIDgenerate()
  
  # Extract original folder name
  base_name <- basename(src_folder)
  random_folder_name<-paste0("p",id, "_", base_name)
  
  # Create new folder name
  new_folder <- file.path(src_path, random_folder_name)
  cat("creating new dir in",new_folder,"\n")
  # Recursively copy
  dir.create(new_folder)
  file.copy(list.files(source_actual, full.names = TRUE), new_folder, recursive = TRUE)
  
  return(random_folder_name)
}

is_image <- function(filename) {
  # Extract the extension
  ext <- tolower(tools::file_ext(filename))
  
  # Check if it is one of the common image formats
  ext %in% c("png", "jpg", "jpeg", "gif", "bmp", "tiff")
}

execute<-function(script_folder,params,process_to_call,process_folder,wrapper_folder){
  
  cat("#calling the external process\n")

  cat("Preparing sandbox..\n")
  previous_process_folder<-process_folder
  process_folder <- duplicate_folder_with_uuid(script_folder,process_folder)
  cat("Sandbox done: ", process_folder, "\n")
  cat("Calling",paste0(wrapper_folder,process_to_call),"\n")
  source(paste0(wrapper_folder,process_to_call), local = TRUE)
  
  script_sandbox<-paste0(script_folder,process_folder)
  expected_outputs<-doCall(script_sandbox,params)
  cat("#process called\n")
  cat("output:",expected_outputs,"\n")
  return(list(
    expected_outputs=expected_outputs,
    process_folder=process_folder))
}

erase<-function(processes_folder,script_folder){
  
  cat("erasing folder\n")
  
  is_p_uuid <- grepl(
    "^p[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}",
    script_folder
  )
  
  foldertodel<-paste0(processes_folder,"/",script_folder)
  
  if (is_p_uuid && dir.exists(foldertodel)) {
    cat("cleaning all in process script folder",script_folder,"\n")  
    # Delete all files inside, but keep the folder
    unlink(foldertodel, recursive = TRUE)
  }
  
  cat("erasing done\n")
}

cleanup<-function(www_folder,processes_folder,params=NULL){
  
  cat("cleaning all in process folder",www_folder,"\n")
  if (dir.exists(www_folder)) {
    files <- list.files(www_folder, full.names = TRUE)
    
    for (f in files) {
      if (file.exists(f)) {
        mod_time <- file.info(f)$mtime
        cat("process",f,"was last modified on",mod_time," \n")
        age_days <- as.numeric(difftime(Sys.time(), mod_time, units = "days"))
        cat("age: ",age_days,"days \n")
        
        if (age_days > 1) {
          unlink(f,recursive = T)  # delete the file
          cat("Deleted old folder:", f, "\n")
        }
      }
    }
  }
  
  cat("erasing hung processes in",processes_folder,"\n")
  
  if (dir.exists(processes_folder)) {
    filesProc <- list.files(processes_folder, full.names = TRUE)
    
    for (f in filesProc) {
      if (file.exists(f)){
        is_p_uuid <- grepl(
          "^p[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}",
          basename(f)
        )
        if (is_p_uuid){
          mod_time <- file.info(f)$mtime
          cat("process hung",f,"was last modified on",mod_time," \n")
          age_days <- as.numeric(difftime(Sys.time(), mod_time, units = "days"))
          cat("age: ",age_days,"days \n")
          
          if (age_days > 1) {
            unlink(f,recursive = T)  # delete the file
            cat("Deleted old process:", f, "\n")
          }
        }}
    }
  }
  cat("cleaning done\n")
}
