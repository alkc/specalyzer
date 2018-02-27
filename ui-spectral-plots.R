get_spectral_plot_ui <- function() {
  fluidPage(
    titlePanel("Spectral Plots"),
    sidebarLayout(
      sidebarPanel(
        selectInput("spec_plot_type", "What would you like to plot?",
                    c("Mean reflectance" = "mean",
                      "Median reflectance" = "median",
                      "Reflectance variance" = "var",
                      # "Reflectance standard deviation" = "sd",
                      "Individual spectra" = "individual"),
                    selected = "mean"),
        conditionalPanel(
          "input.spec_plot_type != 'individual'",
          selectInput(
            inputId = "spec_plot_by", 
            label = "Choose samples to include in your plot:",
            choices =  c("User-defined subsets" = "samples",
                         "Group by attributes" = "attributes"))),
        conditionalPanel(
          "input.spec_plot_by == 'samples' | input.spec_plot_type == 'individual'",
          selectizeInput(
            "spec_plot_sample_select", 
            label = "Please select spectra:", 
            choices = "", 
            multiple = TRUE,
            options = list(maxItems = 10)
            )
          ),
        conditionalPanel(
          "input.spec_plot_by == 'attributes' & input.spec_plot_type != 'individual'",
          selectizeInput(
            "spec_plot_attribute_select", 
            label = "Please select attribute:", 
            choices = "", 
            multiple = FALSE)
        ),
        textInput("spec_plot_title", label = "Plot title", value = ''),
        textInput("spec_plot_ylab", label = "Y-axis label", value = 'Reflectance'),
        textInput("spec_plot_xlab", label = "X-axis label", value = 'Wavelength')
      ),
      mainPanel(
        plotlyOutput("spectral_plot", height = "700px")
      )
    )
  )
}