get_vegindex_selection_plot_ui <- function() {
  fluidPage(
    titlePanel("Selection of vegetation indices"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "viselection_attr_select",
          label = "Select attribute of interest:",
          choices = "",
          multiple = FALSE,
          selected = NULL
        ),
        selectInput(
          "viselection_attr_type",
          label = "Select attribute:",
          choices = c("Continuous" = "continuous", "Categorical" = "categorical"),
          multiple = FALSE,
          selected = NULL
        )
      ),
      mainPanel(
        plotlyOutput("viselection_plot", height = "700px")
      )
    )
  )
}