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
header<- properties[["header"]]
description<- properties[["description"]]
script_folder<<-"./process/"

cb_list<-list(useShinyjs())
all_inputs<-c()
needs_map<<-F
bounding_box_input_var<<-""

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
    all_inputs<-c(all_inputs,key)
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
    all_inputs<-c(all_inputs,key)
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
    all_inputs<-c(all_inputs,key)
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
    all_inputs<-c(all_inputs,key)
    cb_list[[length(cb_list) + 1]] <- i
  }
}

cbl <- do.call(card_body, cb_list)

ui <- grid_page(
  
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
    all_filled <- TRUE
    
    for (k in 1:length(all_inputs)){
      key <- all_inputs[k]
      val <- input[[key]]
      #cat("v:",val," k:",key,"\n")
      if (is.null(val) || val == "" || (is.numeric(val) && is.na(val))) {
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
    params<-c()
    for (k in 1:length(all_inputs)){
      key <- all_inputs[k]
      val <- input[[key]]
      params<-c(params,val)
    }
    
    cat("params:",paste(params),"\n")   # for debugging
    cleanup(script_folder,params)
    expected_outputs<-execute(script_folder,params)
    
    downloadResults(expected_outputs,output)
    
    showNotification("Computation finished", type = "message")
    
  })
}


cleanup<-function(script_folder,params=NULL){
  www_folder<-"www"
  cat("cleaning all in process folder",script_folder,"\n")
  if (dir.exists(www_folder)) {
    
    # Delete all files inside, but keep the folder
    unlink(list.files(www_folder, full.names = TRUE), recursive = TRUE)
  }
  
  aux_files <- list.files(script_folder, pattern = "\\.(csv|png|dat)$", full.names = TRUE)
  
  # Delete them
  if (length(aux_files) > 0) {
    unlink(aux_files)
  }
  cat("cleaning done\n")
}

execute<-function(script_folder,params){
  
  cat("#calling the external process\n")
  source("callProcess.R", local = TRUE)
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
    showNotification("Computation finished", type = "message")
    
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
