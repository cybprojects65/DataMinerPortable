library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(jsonlite)
library(shinyjs)

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

properties <<- read_properties("config.properties")
header<<- properties[["header"]]
description<<- properties[["description"]]
process_to_call<<- properties[["process"]]

script_folder<<-"./processes/"
local_tmp_data_folder<<-"./tmp_data/"
cb_list<<-list(useShinyjs())
all_inputs<<-c()
needs_map<<-F
bounding_box_input_var<<-""
input_file_vars<<-c()
input_file_vars_uploaded<<-c()
column_vars<<-c()
column_vars_references<<-c()

for (k in 1:length(properties)){
  key <- names(properties)[k]
  v <- properties[[k]]
  if (grepl("text_area_*", key, perl = TRUE)){
    parts <- strsplit(v, ",")[[1]]
    # Remove quotes
    parts <- gsub('^"|"$', '', parts)
    lab<-parts[1]
    val<-parts[2]
    i<-textInput(
      inputId = key,
      label = lab,
      value = val,
      width = "100%"
    )
    all_inputs<<-c(all_inputs,key)
    cb_list[[length(cb_list) + 1]] <- i
      
  }else if (grepl("numeric_*", key, perl = TRUE)){
    parts <- strsplit(v, ",")[[1]]
    # Remove quotes
    parts <- gsub('^"|"$', '', parts)
    lab<-parts[1]
    val<-parts[2]
    i<-numericInput(
      inputId = key,
      label = lab,
      value = val
    )
    all_inputs<<-c(all_inputs,key)
    cb_list[[length(cb_list) + 1]] <- i
  } else if (grepl("select_input_*", key, perl = TRUE)){
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
      selected = def
    )
    all_inputs<<-c(all_inputs,key)
    cb_list[[length(cb_list) + 1]] <- i
  }  else if (grepl("bounding_box_*", key, perl = TRUE)){
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
    
    i<-textInput(
      inputId = key,
      label = lab,
      value = val,
      width = "100%"
    )
    bounding_box_input_var<<-key
    needs_map<<-T
    all_inputs<<-c(all_inputs,key)
    cb_list[[length(cb_list) + 1]] <- i
  } else if (grepl("file_input*", key, perl = TRUE)){
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
                ".csv", ".txt", ".json", ".zip", ".bin", ".dat", ".mat"
              ))
    
    input_file_vars<<-c(input_file_vars,key)
    input_file_vars_uploaded<<-c(input_file_vars_uploaded,NA)
    all_inputs<<-c(all_inputs,key)
    cb_list[[length(cb_list) + 1]] <- i
  }else if (grepl("column_selection_*", key, perl = TRUE)){
    parts <- strsplit(v, ",")[[1]]
    parts <- gsub('^"|"$', '', parts)
    reference<-parts[1]
    lab<-parts[2]
    
    i<-checkboxGroupInput(
      inputId = key,
      label = lab,
      choices = list()
    )
    
    column_vars<<-c(column_vars,key)
    column_vars_references<<-c(column_vars_references,reference)
    
    all_inputs<<-c(all_inputs,key)
    cb_list[[length(cb_list) + 1]] <- i
  }
  
  
  
}

cbl <- do.call(card_body, cb_list)

ui <- grid_page(
  tags$head(
    tags$title("DM portable")  # Browser tab title
  ),
   #h2("DM portable verion"),
  
  layout = c(
    "header    header",
    "subheader subheader",
    "sidebar   plot"
  ),
  row_sizes = c(
    "50px",   # header height
    "70px",   # subtitle height
    "1fr"     # main content
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  
  
  # Sidebar card (unchanged)
  grid_card(
    area = "sidebar",
    card_header("Parameters"),
    card_body(
        cbl,
        actionButton("reset_bb", "Reset", width = "50%", class = "btn btn-primary"),
        actionButton("start_computation", "Execute", class = "btn btn-secondary",disabled = "disabled")
    )
  ),
  
  # Title (first row)
  grid_card_text(
    area = "header",
    content = tags$div(header,
                       style = "font-size: 1.25rem; font-weight: 700; line-height:1;"),
    alignment = "start",
    is_title = TRUE
  ),
  
  # Subtitle on its own row (second row)
  grid_card_text(
    area = "subheader",
    content = tags$div(description,
                       style = "
      font-size: 0.70rem; 
      color: #6c757d; 
      line-height: 1;
    "
                       
    ),
    alignment = "start",
    is_title = FALSE
  ),
  
  # Map card (main content)
  grid_card(
    area = "plot",
    #card_header("Bounding box selection"),
    card_header(uiOutput("plot_header")),
    card_body(
      uiOutput("map_or_image")
    )

  )
  
  
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        polygonOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = drawRectangleOptions()
      )
  })
  
  output$map_or_image <- renderUI({
    if(needs_map){  
      leafletOutput("map", height = "400px")
    }
    })
  
  observeEvent(input$reset_bb, {
    # Reload the entire page
    runjs("location.reload();")
  })
  
  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    
    if (!is.null(feat)) {
      geojson_text <- jsonlite::toJSON(list(
        type = "FeatureCollection",
        features = list(list(type="Feature", geometry=feat$geometry, properties=feat$properties))
      ), auto_unbox = TRUE)
      
      geom <- sf::st_read(geojson_text, quiet = TRUE)
      wkt_text <- sf::st_as_text(geom$geometry[1])
      cat("updating\n")
      updateTextInput(session, bounding_box_input_var, value = wkt_text)
      leafletProxy("map") %>% removeDrawToolbar()
    }
  })
  
  # Enable/disable Execute button
  observe({
    #check for file uploading
    if (length(input_file_vars)>0){
      #check for input update
      
      #else
      cat("Checking for uploaded inputs\n")
      fidx<-1
      for (file in input_file_vars){
        req(input[[file]])
        uploader_path <- input[[file]]$datapath
        
        current_file<-input_file_vars_uploaded[fidx]
        if (is.na(current_file) || current_file!=uploader_path){
          #cat("current file:", current_file,"vs", uploader_path,"\n")
          input_file_vars_uploaded[fidx]<<-uploader_path
          cat("User uploaded:", uploader_path, "\n")
          local_path<-paste0(local_tmp_data_folder, input[[file]]$name)
          
           ref<-1
          for (reference in column_vars_references){
            cat("filecheck:",reference,"vs",file,"\n")
            if (reference==file){
              itemid<-column_vars[ref]
              #cat("itemid:",itemid,"\n")
              if ( (tolower(tools::file_ext(uploader_path)) == "csv") && (file.size(uploader_path)>0)
                  ){
                headers <- names(read.csv(uploader_path, nrows = 0))
                updateCheckboxGroupInput(
                session,
                inputId = itemid,
                choices = headers, #c("X", "Y", "Z"),    # new choices
                selected = headers[1]                 # optional default selection
                )
              }
            }
            ref<-ref+1
          }#end for on references
        }#end if check on the current file update
        
       fidx<-fidx+1
      }
    }#end check on the existence of files to upload
    
    
    all_filled <- TRUE
    print(all_inputs)
    for (k in 1:length(all_inputs)){
      key <- all_inputs[k]
      cat("k:",key,"\n")
      val <- input[[key]]
      #cat("v:",val,"\n")
      print(val)
      cat(dim(val))
      
      if (is.null(val) || (is.data.frame(val) && dim(val)[1]==0) || (is.vector(val) && length(val)==0)
          || (is.character(val) && nchar(paste0(val,collapse = ""))==0) || (is.numeric(val) && is.na(val))) {
        all_filled <- FALSE
        break
      }
    }
    
    if (all_filled) {
      enable("start_computation")
      runjs("$('#start_computation').removeClass('btn-secondary').addClass('btn-primary')")
    } else {
      disable("start_computation")
    }
    
    
    
    
  })
  
  computing <- reactiveVal(FALSE)
  # --- Call process when Execute is clicked ---
  
  observeEvent(input$start_computation, {
    
    showNotification("Computation started...", type = "message")
    params<<-list()
    for (k in 1:length(all_inputs)){
      key <- all_inputs[k]
      val <- as.character(input[[key]])
      params[[k]]<<-val
    }
    
    cat("params:",paste(params),"\n")   # for debugging
    cleanup(script_folder,params)
    expected_outputs<-execute(script_folder,params)
    
    downloadResults(expected_outputs,output)
    
    showNotification("Computation finished", type = "message")
    
  })

  #file uploading
  
  
  
}


cleanup<-function(script_folder,params=NULL){
  www_folder<-"www"
  cat("cleaning all in process folder",script_folder,"\n")
  if (dir.exists(www_folder)) {
    
    # Delete all files inside, but keep the folder
    unlink(list.files(www_folder, full.names = TRUE), recursive = TRUE)
  }
  
  aux_files <- list.files(script_folder, pattern = "\\.(csv|png|dat|Rdata|txt)$", full.names = TRUE)
  
  # Delete them
  if (length(aux_files) > 0) {
    unlink(aux_files)
  }
  cat("cleaning done\n")
}

execute<-function(script_folder,params){
  
  cat("#calling the external process\n")
  #source("callProcess.R", local = TRUE)
  source(process_to_call, local = TRUE)
  expected_outputs<-doCall(script_folder,params)
  cat("#process called\n")
  cat("output:",expected_outputs,"\n")
  return(expected_outputs)
}

is_image <- function(filename) {
  # Extract the extension
  ext <- tolower(tools::file_ext(filename))
  
  # Check if it is one of the common image formats
  ext %in% c("png", "jpg", "jpeg", "gif", "bmp", "tiff")
}

downloadResults<- function(expected_outputs,output){

  if (length(expected_outputs)>0){
    #showNotification("Computation finished", type = "message")
    
    tl<-tagList()
    idx<-1
    for(outputf in expected_outputs){
      out_path <- paste0(script_folder, outputf)
      if (file.exists(out_path)){
        cat("copying ",out_path,"\n")
        file.copy(file.path(out_path), paste0("www/",outputf), overwrite = TRUE)
        if (is_image(out_path)){
          sub_tag<-tagList(
            tags$img(src = outputf, width = "100%", style = "max-height:400px; object-fit:contain;"),
            tags$div(style = "margin-top: 1rem;"),
            downloadButton(paste0("download_",idx), paste0("Download ",outputf)),
            tags$div(style = "margin-top: 1rem;")
          )
          
        }else{
          sub_tag<-tagList(
            downloadButton(paste0("download_",idx), paste0("Download ",outputf)),
            tags$div(style = "margin-top: 1rem;")
          )  
        }
        
        tl<-tagList(tl,sub_tag)
        idx<-idx+1
      }
    }
    
    #cat("\n",full_path_to_image, "\n")
    
    output$plot_header <- renderUI({
      "Results"
    })
    
    
    output$map_or_image <- renderUI({
      tl
    })
    
    idx<-1
    for(outputf in expected_outputs){
      if (file.exists(out_path)){
        cat(paste0("download_", idx),"\n")
        cat(paste0("outputf: ", outputf),"\n")
        
        local({
          idx_fixed<-idx
          outputf_fixed<-outputf
          output[[paste0("download_", idx_fixed)]] <- downloadHandler(
            filename = function() basename(outputf_fixed),
            content = function(file) {
              file.copy(file.path("www", outputf_fixed), file, overwrite = TRUE)
            }
          )
        })
        
        idx<-idx+1 
      }  
    }#downloading
    
  }
  
    
}

shinyApp(ui, server)
