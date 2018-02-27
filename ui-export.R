get_export_data_ui <- function() {
  fluidPage(
    titlePanel("Export data"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId  = "export_select_table_type",
          label = "What do you want to export?",
          choices = c("Spectral data" = "spectra",
                      "Vegetation index data" = "vegindex")
        ), conditionalPanel(
            condition = "input.export_select_table_type == 'vegindex'",
            selectInput(inputId = "export_table_vegindex_vi",
                        label = "Choose vegetation indices to include in your exported table",
                        choices = "",
                        multiple = TRUE)
        ), conditionalPanel(
            condition = "input.export_select_table_type == 'vegindex'",
            checkboxInput(
              inputId = "export_vegindex_all_indices",
              label = "Include all indices?",
              value = FALSE
          )
        ), checkboxInput(
          inputId = "export_by_attrib",
          label = "Calculate mean indices/spectra by an attribute?",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.export_by_attrib",
          selectInput(
            inputId = "export_by_attrib_select",
            label = "Which attribute?",
            choices = "",
            multiple = FALSE
            )
        ), 
        actionButton("export_preview_table_button", label = "Preview output"),
        downloadButton("export_download_table")
      ),
      mainPanel(
        DT::dataTableOutput("export_table_preview")
      )
    )
  )
}