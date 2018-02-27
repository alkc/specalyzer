get_setup_ui <- function() {
  fluidPage(
    titlePanel("Modify your dataset"),
    sidebarLayout(
      sidebarPanel(
        # Select rows to remove from dataset
        selectInput(
          inputId = "setup_subset_select_rows",
          label = "Select rows to remove from your working dataset",
          choices = "",
          multiple = TRUE),
        # Conditional panels go here:
        textInput(
          inputId = "setup_subset_mask",
          label = "Wavelenght intervals to remove from your working data",
          placeholder = "Example: 300,400,1000,1200"),
        selectInput(
          inputId = "setup_subset_attrib_cols",
          label = "Select attributes you wish to remove from your data",
          choices = "",
          multiple = TRUE),
        actionButton(
          inputId = "setup_subset_preview_changes",
          label = "Preview changes"),
        actionButton(
          inputId = "setup_subset_apply_changes",
          label = "Apply changes"),
        actionButton(
          "setup_subset_restore_defaults",
          label = "Reset inputs")
      ),
      mainPanel(
        tabsetPanel(
          id = "setup_subset_tab",
          tabPanel(
            'Spectra',
            DT::dataTableOutput("setup_subset_spectra_preview")),
          tabPanel(
            'Attributes',
            DT::dataTableOutput("setup_subset_attrib_preview"))
          # tabPanel(
          #   'Field matrices',
          #   DT::dataTableOutput("setup_subset_table_fieldmat")
          #   )
        )
      )
    )
  )
}