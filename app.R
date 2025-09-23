library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(jsonlite)
library(shinyjs)
library(uuid)

#####global parameters
applications<<-c()
app_descriptions<<-c()
config_paths<<-"./configurations/"
script_folder<<-"./processes/"
wrapper_folder<<-"./wrappers/"
public_folder<<-"./www/"
#####READ configuration and build the method list
source("auxfunctions.R")

config_files<-list.files(
  path = paste0(config_paths),
  pattern = "^config_", 
  full.names = TRUE    # TRUE = returns full path, FALSE = just file names
)
cleaned <- sub("^config_", "", basename(config_files))
cleaned <- sub("*\\.properties", "", cleaned)
app_numb<-1
for (f in cleaned){
  applications<<-c(applications,f)
  properties <<- read_properties(config_files[app_numb])
  app_descriptions<<-c(app_descriptions,properties[["description"]])
  app_numb<-app_numb+1
}
#####END READ configuration and build the method list

#build the methods list
methods<-build_method_list(applications,app_descriptions)

#clean old folders
cleanup(public_folder,script_folder)
  
ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar plot  "
  ),
  row_sizes = c(
    "80px",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  #SIDEBAR
  grid_card(
    area = "sidebar",
    card_header("Methods"),
    card_body(
      methods
    )
  ),
  #HEADER
  grid_card_text(
    area = "header",
    content = "DataMiner",
    alignment = "start",
    is_title = FALSE
  ),
  
  #PLOT AREA
  grid_card(
    area = "plot",
    card_header("Method parameters"),
    card_body(
      uiOutput("method_ui")
    )
  )
)


server <- function(input, output, session) {
  cat("###session:")
  print(session$token)
  cat("###")
  
  #session variables
  selected_method <- reactiveVal(NULL)
  input_values_fulfilled<-reactiveVal(NULL)
  input_parameters_session <- reactiveVal(NULL)
  reset_trigger <- reactiveVal(0)
  
  #build observer for the methods in the left panel
  for (i in 1:length(methods)) {
    local({
      action_id <- paste0("method_", i)
      observeEvent(input[[action_id]], {
        selected_method(action_id)
      })
    })
  }
  
  
  observeEvent(input$reset_bb, {
    cat("Reset pressed\n")
    # "Re-select" the current method to trigger the same logic as clicking a sidebar button
    #dummy <- "RESET"
    #current(selected_method())
    #isolate({
    # selected_method(dummy)
    #})
    #TODO: write current method locally and read it
    
    runjs("location.reload();")
    
    #selected_method(current())
  })
  
  
  # Build the method UI after method selection (right panel)
  output$method_ui <- renderUI({
    
    req(selected_method()) #triggered by the method selection
    reset_trigger()
    
    cat("\nrendering",selected_method(),"\n")
    input_parameters <- build_input_list(applications, selected_method(), config_paths)
    input_parameters_session(input_parameters)
    
    
    tagl <- list(
      tags$h2(style = "color:#2c3e50; font-weight:bold; margin-top:0.5em; margin-bottom:0.5em;", input_parameters$header),
      tags$p(class = "fst-italic text-muted mb-4", input_parameters$description),
      input_parameters$cb_list,
      actionButton("reset_bb", "Reset", width = "20%", class = "btn btn-primary"),
      actionButton("start_computation", "Execute",
                   style = "display:block; margin-top:10px;", class = "btn btn-secondary",
                   width = "50%", disabled = "disabled"),
      uiOutput("results_panel")
    )

    tagl
  })
  
  # Render maps
  observe({
    req(input_parameters_session())
    all_inp <- input_parameters_session()$all_inputs
    
    for (i in all_inp) {
      if (grepl("_map$", i)) {
        cat("recreating renderings for map",i,"\n")
        local({
          map_id <- i
          output[[map_id]] <- renderLeaflet({
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
        })
      }
    }
  })
  
  
  # entangle map and bounding box values
  observe({
    req(input_parameters_session())
    all_inp <- input_parameters_session()$all_inputs
    
    for (i in all_inp) {
      if (grepl("_map$", i)) {
        cat("recreating observer for map",i,"\n")
        local({
          map_id <- i
          map_event <- paste0(map_id, "_draw_new_feature")
          bounding_box_input_var <- sub("_map$", "", map_id)
          
         observeEvent(input[[map_event]], {
            feat <- input[[map_event]]
            if (!is.null(feat)) {
              geojson_text <- jsonlite::toJSON(list(
                type = "FeatureCollection",
                features = list(list(type = "Feature", geometry = feat$geometry, properties = feat$properties))
              ), auto_unbox = TRUE)
              
              geom <- sf::st_read(geojson_text, quiet = TRUE)
              wkt_text <- sf::st_as_text(geom$geometry[1])
              
              updateTextInput(session, bounding_box_input_var, value = wkt_text)
              leafletProxy(map_id) %>% removeDrawToolbar()
            }
          }, ignoreInit = TRUE)
        })
      }
    }
  })
  
  
  # build radio and selection items based on table input
  observe({
    req(input_parameters_session())
    input_file_vars<-input_parameters_session()$input_file_vars
    #check for file uploading
    if (length(input_file_vars)>0){
      #check for input update
      cat("Checking for uploaded inputs\n")
      fidx<-1
      for (file in input_file_vars){
        req(input[[file]])
        uploader_path <- input[[file]]$datapath
        cat("User uploaded:", uploader_path, "\n")
        column_vars<-input_parameters_session()$column_vars
        column_vars_references<-input_parameters_session()$column_vars_references
        single_column_vars_references<-input_parameters_session()$single_column_vars_references
        single_column_vars<-input_parameters_session()$single_column_vars
        ref<-1
        for (reference in column_vars_references){
          cat("filecheck ref cols:",reference,"vs",file,"\n")
          if (reference==file){
            itemid<-column_vars[ref]
            if ( (tolower(tools::file_ext(uploader_path)) == "csv") && (file.size(uploader_path)>0)
            ){
              headers <- names(read.csv(uploader_path, nrows = 0))
              updateCheckboxGroupInput(
                session,
                inputId = itemid,
                choices = headers, # new choices
                selected = headers[1]                 # optional default selection
              )
            }
          }
          ref<-ref+1
        }
        ref<-1
        for (reference in single_column_vars_references){
          cat("filecheck ref single col:",reference,"vs",file,"\n")
          if (reference==file){
            itemid<-single_column_vars[ref]
            #cat("itemid:",itemid,"\n")
            if ( (tolower(tools::file_ext(uploader_path)) == "csv") && (file.size(uploader_path)>0)
            ){
              headers <- names(read.csv(uploader_path, nrows = 0))
              updateRadioButtons(
                session,
                inputId = itemid,
                choices = headers,
                selected = headers[1]
              )
            }
          }
           ref<-ref+1
        }
      }
    }#end check on the existence of files to upload
  })

  #observing inputs to activate execution button
  observe({
    req(input_parameters_session())
    
    all_inputs<-input_parameters_session()$all_inputs
    #check for file uploading
    if (!is.null(all_inputs) && length(all_inputs)>0){
      cat("checking if all inputs are fulfilled:\n")     
      all_filled <- TRUE
      filled_params<-list()
      filled_values<-list()
      for (k in 1:length(all_inputs)){
        key <- all_inputs[k]
        val <- input[[key]]
        
        if (grepl("_map$", key))
          next
        
        if (is.null(val) || (is.data.frame(val) && dim(val)[1]==0) || (is.vector(val) && length(val)==0)
            || (is.character(val) && nchar(paste0(val,collapse = ""))==0) || (is.numeric(val) && is.na(val))) {

            all_filled <- FALSE
          break
        }
        filled_params[[length(filled_params)+1]]<-key
        filled_values[[length(filled_values)+1]]<-val
      }
      cat("All filled:",all_filled,"\n")     
      if (all_filled) {
        cat("enabling the button\n")
        enable("start_computation")
        removeClass("start_computation", "btn-secondary")
        addClass("start_computation", "btn-primary")
        input_values_fulfilled(list(
                                  filled_params=filled_params,
                                  filled_values=filled_values,
                                  process_to_call=input_parameters_session()$process_to_call,
                                  process_folder=input_parameters_session()$process_folder
                                    ))
      } else {
        cat("disabling the button\n")
        disable("start_computation")
        removeClass("start_computation", "btn-primary")
        addClass("start_computation", "btn-secondary")
      }
      
    }#end check on input vars
  })
  
  #manage the computation
  observeEvent(input$start_computation, {
    req(input_parameters_session())
    req(input_values_fulfilled)
    showNotification("Computation started...", type = "message")
    
    filled_values<-input_values_fulfilled()
    filled_values_l<-filled_values$filled_values
    params<-list()
    for (k in 1:length(filled_values_l)){
      params[[k]]<-as.character(filled_values_l[[k]])
    }
    
    cat("params:",paste(params),"\n")   # for debugging
    
    ptc<-input_values_fulfilled()$process_to_call
    pf<-input_values_fulfilled()$process_folder
    
    process_outputs<-execute(script_folder,params,ptc,pf,wrapper_folder)
    
    expected_outputs=process_outputs$expected_outputs
    process_folder_4d=process_outputs$process_folder
    
    downloadResults(expected_outputs,output,process_folder_4d)
    
    showNotification("Computation finished", type = "message")
    
  })
}

downloadResults <- function(expected_outputs, output, process_folder) {
  
  if (length(expected_outputs) > 0) {
    
    tl <- tagList()
    idx <- 1
    outfol <- file.path(public_folder, process_folder)
    if (!file.exists(outfol))
      dir.create(outfol, recursive = TRUE)
    
    # PREPARAZIONE DEI BOTTONI
    for (outputf in expected_outputs) {
      out_path <- file.path(script_folder, process_folder, outputf)   # ✅ definito dentro il ciclo
      cat("checking output: ", file.exists(out_path), "\n")
      if (file.exists(out_path)) {
        cat("copying ", out_path, "\n")
        target_out <- file.path(outfol, basename(outputf))
        file.copy(out_path, target_out, overwrite = TRUE)
        
        if (is_image(out_path)) {
          pathtoimg<-paste0("/",file.path(process_folder, basename(outputf)))
          cat("path to img",pathtoimg,"\n")
          sub_tag <- tagList(
            tags$img(src = pathtoimg, 
                     width = "100%", 
                     style = "max-height:400px; object-fit:contain;"),
            tags$div(style = "margin-top: 1rem;"),
            downloadButton(paste0("download_", idx), basename(outputf)),
            tags$div(style = "margin-top: 1rem;")
          )
        } else {
          sub_tag <- tagList(
            downloadButton(paste0("download_", idx), basename(outputf)),
            tags$div(style = "margin-top: 1rem;")
          )
        }
        
        tl <- tagList(tl, sub_tag)
        idx <- idx + 1
      }
    }
    
    output$results_panel <- renderUI({
      tags$div(
        id = "results_panel",
        tags$h3("Results", style = "color:#2c3e50; font-weight:bold; margin-bottom:1rem;"),
        tl
      )
    })
    # HANDLER PER IL DOWNLOAD
    idx <- 1
    for (outputf in expected_outputs) {
      out_path <- file.path(script_folder, process_folder, outputf)   # ✅ idem qui
      if (file.exists(out_path)) {
        local({
          idx_fixed <- idx
          outputf_fixed <- basename(outputf)
          output[[paste0("download_", idx_fixed)]] <- downloadHandler(
            filename = function() outputf_fixed,
            content = function(file) {
              file.copy(file.path(outfol, outputf_fixed), file, overwrite = TRUE)
            }
          )
        })
        idx <- idx + 1
      }
    }
    
   runjs("
    var el = document.getElementById('results_panel');
    if(el){
      el.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
    ")
   
    erase(script_folder, process_folder)
  }
}



shinyApp(ui, server)
  

