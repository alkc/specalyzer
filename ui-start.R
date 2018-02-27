get_start_ui <- function() {
  fluidPage(
    titlePanel("Upload"),
    sidebarLayout(
      sidebarPanel(
        fileInput(
          inputId = "spectral_data_upload", 
          label = "Select spectral files",
          multiple = TRUE),
        fileInput(
            inputId = "attrib_data_upload", 
            label = "Select attribute data (optional)",
            multiple = FALSE,
            accept = "text/csv"),
        selectInput(
          inputId = "spectral_data_type",
          label = "File types",
          choices = c("Spectrawiz (.TRM, .IRR, .etc)" = "spectrawiz",
                      "ASD Binary files" = "asd",
                      "Long form" = "long",
                      "Wide form" = "wide"),
          multiple = FALSE),
        fileInput(
          inputId = "fieldmap_upload", 
          label = "Select fieldmap data (optional)",
          multiple = TRUE,
          accept = "text/csv"),
        
        actionButton(
          inputId = "spectral_data_upload_button",
          label = "Process uploaded data"
        ),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),
        actionButton(
          inputId = "upload_reset_button",
          label = "Reset"
        )
      ),
      mainPanel(
        htmlOutput("welcome_text"),
	downloadLink("download_example_data", "Download")
      )
    )
  )
}