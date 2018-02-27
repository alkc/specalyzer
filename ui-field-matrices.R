get_field_matrix_ui <- function() {
  fluidPage(titlePanel("Fieldmap plots"),
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  "plot_field_matrix",
                  label = "Choose a field layout:",
                  choices = "",
                  selected = NULL
                ),
                selectInput(
                  "plot_field_type",
                  label = "Choose fieldmap type:",
                  choices = c("Vegetation index" = "vegindex",
                              "Attributes" = "attribute"),
                  selected = NULL
                ),
                conditionalPanel(
                  condition = "input.plot_field_type == 'vegindex'",
                  selectInput(
                    "plot_field_index",
                    label = "Choose an index:",
                    choices = "",
                    selected = NULL
                  ),
                  checkboxInput("plot_field_custom", label = "Use a custom vegetation index?", value = FALSE)
                ),
                conditionalPanel(
                  condition = "input.plot_field_type == 'attribute'",
                  selectInput(
                    "plot_field_attribute",
                    label = "Choose an attribute:",
                    choices = "",
                    selected = NULL
                  )
                ),
                # checkboxInput("plot_field_custom", label = "Use a custom vegetation index?", value = FALSE),
                conditionalPanel(
                  condition = "input.plot_field_custom",
                  textInput(
                    inputId = "plot_field_custom_index",
                    label = "Enter a custom veg index",
                    value = "(R800 - R650)/(R650 + R800)"
                  )
                ),
                textInput(inputId = "plot_field_title",
                          label = "Plot title")
              ),
              mainPanel(plotlyOutput("field_matrix_plot", height = "700px"))
            ))
}