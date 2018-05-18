get_vegindex_plot_ui <- function() {
  fluidPage(
    titlePanel("Vegetation index visualization"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "vi_index_select",
          label = "Select vegetation index:",
          choices = "",
          multiple = FALSE
        ),
        checkboxInput("vi_plot_custom_index", label = "Use a custom vegetation index?", value = FALSE),
        conditionalPanel(
          condition = "input.vi_plot_custom_index",
          textInput(
            inputId = "vi_custom_index_input", 
            label ="Enter a custom veg index", 
            value = "(R800 - R650)/(R650 + R800)")
        ),
        # selectInput(
        #   "vi_plot_by",
        #   label = "Select plot type:",
        #   choices = c(
        #     "Compare vegetation index by attribute" = "attribute",
        #     "Compare vegetation index by sample" = "sample"
        #   ),
        #   selected = "attribute"
        # ),
          selectInput(
            "vi_plot_attribute_select",
            label = "Select attribute:",
            choices = "",
            multiple = FALSE
          ),
          selectInput(
            "vi_attrib_plot_type",
            label = "Choose plot type:",
            choices = c(
              "Box plot (for ordinal or nominal attributes)" = "boxplot",
              "Scatter plot (for continuous attributes)" = "scatter",
              "Show indices for all samples" = "all"
              )
            ),
        # conditionalPanel(
        #   "input.vi_plot_by == 'sample'",
        #   selectInput(
        #     "vi_plot_sample_select",
        #     label = "Select sample:",
        #     choices = "",
        #     multiple = FALSE
        #   )
        # ),
        conditionalPanel(
          "input.vi_attrib_plot_type == 'boxplot'",
          selectInput(
            "vi_plot_boxplot_boxpoints",
            label = "Show points?",
            choices = list(
              "None" = "none",
              "All points" = "all",
              "Outliers" = "outliers",
              "Suspected outliers" = "suspectedoutliers"
            ),
            selected = "none"),
          selectInput(
            "vi_plot_boxplot_attribute_splitby",
            label = "Split by a 2nd attribute?",
            choices = "",
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.vi_attrib_plot_type == 'scatter'",
          selectInput(
            "vi_plot_scatter_size",
            label = "Map marker color to attribute:",
            choices = "",
            multiple = FALSE
          )
        ),
        textInput(
          inputId = "vi_plot_title",
          label = "Plot title"
        )
      ),
      mainPanel(
        plotlyOutput("vi_plot", height = "700px")
      )
    )
  )
}