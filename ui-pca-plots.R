get_pca_plot_ui <- function() {
  fluidPage(
    titlePanel("PCA Plots"),
    
    sidebarLayout(
      
      sidebarPanel(
        
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
        ),
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