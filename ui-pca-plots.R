get_pca_plot_ui <- function() {
  fluidPage(
    titlePanel("PCA Plots"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "plot_pca_type",
          label = "Visualize PCA plot by outliers or attributes?",
          choices = c("Outliers" = "outliers", "Attributes" = "attributes"),
          selected = "outliers"),
        conditionalPanel("input.plot_pca_type == 'attributes'",
                         selectInput(
                           "plot_pca_color",
                           label = "Color markers by attribute?",
                           choices = "",
                           selected = NULL
                         ),
                         selectInput(
                           "plot_pca_size",
                           label = "Map marker size to attribute?",
                           choices = "",
                           selected = NULL
                         )
        ),
        conditionalPanel(
          "input.plot_pca_type == 'outliers'",
          sliderInput("plot_pca_outlier_sd", label = "Standard devs",
                      min = 1L, max = 10L, step = 1L, value = 3L)),
        # TODO: Add explaining text here:
        checkboxInput(
          inputId = "plot_pca_scale",
          label = "Scale",
          value = TRUE
        ),
        checkboxInput(
          inputId = "plot_pca_center",
          label = "Center",
          value = TRUE
        ),
        textInput(
          inputId = "plot_pca_title",
          label = "Set custom plot title"
        )
      ),
      mainPanel(
        plotlyOutput("pca_plot", height = "700px")
      )
    )
  )
}