get_datatables_ui <- function() {
  sidebarLayout(
    sidebarPanel(
      conditionalPanel("input.table_tabs == 'Vegetation Indices'",
                       selectInput("table_vegindex_select", 
                                   label = "Select indices",
                                   choices = "",
                                   multiple = TRUE),
                       selectInput("table_vegindex_row_select", 
                                   label = "Selected rows",
                                   choices = "",
                                   multiple = TRUE)
                       
      ),
      conditionalPanel("input.table_tabs == 'Attributes'",
                       selectInput("table_attribute_row_select", 
                                   label = "Selected rows",
                                   choices = "",
                                   multiple = TRUE),
                       selectInput("table_attribute_column_select", 
                                   label = "Selected attributes to show",
                                   choices = "",
                                   multiple = TRUE)
      ),
      conditionalPanel("input.table_tabs == 'Spectra'",
                       selectInput("table_vegindex_select", 
                                   label = "Selected rows",
                                   choices = "",
                                   multiple = TRUE)
      )
    ),
    mainPanel(
      
      tabsetPanel(id = "table_tabs",
                  tabPanel('Spectra',
                           DT::dataTableOutput(
                             "table_spectra"
                           )),
                  tabPanel('Attributes',
                           DT::dataTableOutput("table_attributes"
                           )),
                  tabPanel('Vegetation Indices',
                           DT::dataTableOutput("table_vegindex"))
      )
    )
  )
}