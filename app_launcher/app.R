library(shiny)
library(plotly)
library(gridlayout)
library(bslib)

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

config_files<-list.files(
  path = "../",          # or specify another folder
  pattern = "^config_", 
  full.names = TRUE    # TRUE = returns full path, FALSE = just file names
)

cleaned <- sub("^config_", "", basename(config_files))
cleaned <- sub("*\\.properties", "", cleaned)
applications<<-c()
app_descriptions<<-c()
app_numb<-1
for (f in cleaned){
  applications<<-c(applications,f)
  properties <<- read_properties(config_files[app_numb])
  
  app_descriptions<<-c(app_descriptions,properties[["description"]])
  
  app_numb<-app_numb+1
}


ui <- grid_page(
  layout = c(
    "header  header ",
    "sidebar sidebar"
  ),
  row_sizes = c(
    "100px",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Method selection"),
    card_body(
      selectInput(
        inputId = "mselection",
        label = "Select the method to execute", #app_descriptions[1],
        choices = as.list(applications),
        selected = "Ideal",
        width = "100%"
      ),
      uiOutput("method_desc"),  # placeholder for dynamic description
      #em(app_descriptions[1]),
      actionButton(inputId = "launch_method", label = "Launch method", class = "btn btn-primary")
    )
  ),
  grid_card_text(
    area = "header",
    content = "Method launcher for DataMiner-Portable",
    alignment = "start",
    is_title = FALSE
  )
)


server <- function(input, output) {
   
  output$method_desc <- renderUI({
    req(input$mselection)
    idx<-which(input$mselection==applications)
    em(app_descriptions[idx])
  })
  
  observeEvent(input$launch_method, {
    idx <- which(input$mselection == applications)
    header_file <- config_files[idx]

    cmd <- paste0("Rscript -e \"library(shiny); setwd('../'); options(APP_HEADER='",basename(header_file),"', APP_FOLDER='./'); shiny::runApp('./', launch.browser = TRUE)\"")
    cat("App command to launch",cmd,"\n")
    # Launch asynchronously
    system(cmd, wait = F)

    showNotification("Method launched!", type = "message")
  })

}
shinyApp(ui, server)
  

