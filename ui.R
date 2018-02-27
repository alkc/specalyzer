library(plotly)
library(shiny)
source("ui-spectral-plots.R")
source("ui-vegindex-plots.R")
source("ui-datatables.R")
source("ui-pca-plots.R")
source("ui-setup.R")
source("ui-export.R")
source("ui-field-matrices.R")
source("ui-about.R")
source("ui-start.R")

title_thingie <- HTML("<a style=\"text-decoration:none; color:white;\" href=\"http://specalyzer.org\">Specalyzer</a>")

shinyUI(
    navbarPage(title= title_thingie,
               tabPanel(
                 title = "Start", 
                 get_start_ui()
               ),
               tabPanel(
                 title = "Filtering", 
                 get_setup_ui()
               ),
               navbarMenu(
                 title = "Visualize",
                          tabPanel("Spectral Plots", get_spectral_plot_ui()),
                          tabPanel("Vegetation index plots", get_vegindex_plot_ui()),
                          tabPanel("PCA plots", get_pca_plot_ui()),
                          tabPanel("Fieldmaps", get_field_matrix_ui()),
                          tabPanel("Tables", get_datatables_ui())
                 ),
               navbarMenu(
                 title = "Export",
                 tabPanel("Data", get_export_data_ui())
                  # tabPanel("Plots", get_export_plot_ui())
               ),
               tabPanel(
                 title = "About",
                 get_about_ui()
                 # tabPanel("Plots", get_export_plot_ui())
               ),inverse = TRUE))