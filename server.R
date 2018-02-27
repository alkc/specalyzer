library(dplyr)
library(DT)
library(specalyzer)
library(plotly)
library(readr)
library(shiny)
library(shinyjs)

source("server-plots.R")
source("server-export.R")
source("server-process-spectral-data.R")
source("server-process-fieldmap-data.R")
source("server-get-text-summary.R")
source("util.R")

# options(shiny.fullstacktrace = TRUE)

available_indices <- specalyzer::vegindex()

# dev_mode <- FALSE

user_data_base <- file.path("/path/to/specalyzer/user-data-dir")
example_data_path <- "/path/to/specalyzer-example-data.zip"

shinyServer(function(input, output, session) {
  user_data <- reactiveValues(id = NULL, path = NULL, available_samples = NULL, available_fieldmaps = NULL)
  output$download_example_data <- downloadHandler(
      filename <- function(){
          paste("specalyzer-example-data", "zip", sep = ".")
      },
      content <- function(file) {
          file.copy(example_data_path, file)
      },
      contentType = "application/zip"
      )
  observeEvent(input$upload_reset_button, {
    shinyjs::js$refresh()
  })
  
  # File upload -------------------------------------------------------------
  
  observeEvent(input$spectral_data_upload_button, {
    new_unique_id <- stringi::stri_rand_strings(1, 6)
    # Create speclib object
    speclib <- process_spectral_data(
      input_files = input$spectral_data_upload,
      file_format = input$spectral_data_type
    )
    
    if (!is.null(input$attrib_data_upload)) {
      speclib <- process_attribute_data(input$attrib_data_upload, speclib)      
    }
    user_data_path <- file.path(user_data_base, "user_data", new_unique_id)
    dir.create(user_data_path, recursive = TRUE)
    saveRDS(speclib, file.path(user_data_path, "userdata.rds"))
    # Save to uniq folder
    # Set url to uniq job id
    user_data$id <- new_unique_id
    user_data$path <- file.path(user_data_path, "userdata.rds")
    user_data$available_samples <- hsdar::idSpeclib(speclib)
    if (!is.null(input$fieldmap_upload)) {
      uploaded_maps <- input$fieldmap_upload
      fieldmap_names <- unlist(uploaded_maps[['name']])
      fieldmap_paths <- uploaded_maps[['datapath']]
      names(fieldmap_paths) <- fieldmap_names
      updateSelectInput(session, inputId = "plot_field_matrix", choices = fieldmap_paths)
    }    
  })
  
  # Reactive data -----------------------------------------------------------
  
  # Reactive element to return the raw unmodified dataset as a speclib object
  # Returns NULL is no data is loaded
  raw_data <- reactive({
    if(is.null(user_data$path)) {
      return(NULL)
    }
    rds_path <- file.path(user_data$path)
    # print(rds_path)
    readRDS(rds_path)
  })
  
  # the data reactive element returns the uploaded data with any modifications
  # specified by the user in the settings/subset menu.
  data <- reactive({
    validate(
      need(data_exists(), "Please upload your spectral data first.")
    )
    
    dataset <- raw_data()
    
    # TODO: Figure out if this is needed:
    if (is.null(dataset))
      return(NULL)
    
    # data is re-evaluated upon subset-apply button press:
    input$setup_subset_apply_changes
    
    if (input$setup_subset_apply_changes != 0) {
      # Masking data:
      mask_input <- isolate(input$setup_subset_mask)
      mask_intervals <- figure_out_mask(mask_input)
      
      selected_rows <- isolate(input$setup_subset_select_rows)
      
      if (length(selected_rows) > 0) {
        dataset <- remove_selected_rows(dataset, selected_rows)
      }
      
      if (!is.null(mask_intervals)) {
        dataset <- mask_(dataset, mask_intervals)
      }
      # Subset attribute columns
      selected_attrib_cols <-
        isolate(input$setup_subset_attrib_cols)
      if (length(selected_attrib_cols) > 0) {
        attribute(dataset) <-
          get_selected_attribs(dataset, selected_attrib_cols)
      }
    }
    dataset
  })
  
  # Reactive element to that returns TRUE or FALSE depending on if there is
  # any data available.
  data_exists <- reactive({
    !is.null(raw_data())
  })
  
  # Returns a list of available field matrices
  available_field_matrices <- reactive({
    # data_path = file.path(data_path(), "field_matrices/")
    # # print(list.files(data_path))
    # matrices <- list.files(data_path) %>% sapply(FUN = basename)
    # matrices
    NULL
  })
  
  # Used to determine if user included any attribute data with their uploaded
  # data
  attribute_data_exists <- reactive({
    attrib_table <- attribute(data())
    !nrow(attrib_table) == 0
  })
  
  data_spectra <- reactive({
    ids <- idSpeclib(data())
    specdata <- spectra(data())
    rownames(specdata) <- ids
    colnames(specdata) <- wavelength(data())
    specdata
  })
  
  data_attrib <- reactive({
    ids <- idSpeclib(data())
    attribdata <- attribute(data())
    rownames(attribdata) <- ids
    attribdata
  })
  
  # Vegindex table:
  data_vegindex <- reactive({
    selected_vis <- input$table_vegindex_select
    if (is.null(selected_vis)) {
      selected_vis <- c("NDVI", "PRI", "CRI", "MCARI", "WI")
    }
    vi_table <- specalyzer::vegindex(data(), selected_vis)
  })
  
  # Menu item code ----------------------------------------------------------
  
  all_available_samples <- reactive({
    if (is.null(raw_data()))
      return(NULL)
    raw_data() %>% idSpeclib()
  })
  
  all_available_attributes <- reactive({
    # print(attribute_data_exists())
    if (!attribute_data_exists()) {
      return(c("No attributes available" = ""))
    }
    get_names_of_attribs(raw_data())
  })
  
  # Derived from working data:
  available_samples <- reactive({
    data <- data()
    if (is.null(data))
      NULL
    else
      idSpeclib(data())
  })
  
  available_attributes <- reactive({
    get_names_of_attribs(data())
  })
  
  # Plots -------------------------------------------------------------------
  
  output$spectral_plot <- renderPlotly({
    get_spectral_plot(data(), input, output)
  })
  
  output$vi_plot <- renderPlotly({
    get_vi_plot(data(), input, output)
  })
  
  output$pca_plot <- renderPlotly({
    get_pca_plot(data(), input, output)
  })
  
  output$field_matrix_plot <- renderPlotly({
    
    if(input$plot_field_type == 'attribute') {
      
      selected_attribute_is_numeric <-  specalyzer::get_attr_column(data(), input$plot_field_attribute) %>% 
        is.numeric()
  
      validate(
        need(selected_attribute_is_numeric, 
             "specalyzer currently only supports numerical attributes in field plots")
      )
    }
    
    get_field_matrix_plot(dataset = data(),
                          datapath = data_path(),
                          input,
                          output)
  })
  
  # Dynamic ui code ---------------------------------------------------------


 output$welcome_text <- renderUI({
    welcome_text <- includeMarkdown("content/welcome.md")
    if(data_exists()) {
      welcome_text <- get_text_summary(raw_data())
    }
    welcome_text
  })
  
    observeEvent(input$setup_subset_restore_defaults, {
      # print(all_available_attributes())
      updateSelectInput(session, "setup_subset_select_rows", choices = all_available_samples())
      updateSelectInput(session, "setup_subset_attrib_cols", choices = all_available_attributes())
      updateTextInput(session, inputId = "setup_subset_mask", value = "")
    })
    
    observeEvent(input$setup_subset_preview_changes, {
      validate(
        need(data_exists(), "Please upload your spectral data first.")
      )
      
      dataset <- raw_data()
      
      mask_input <- isolate(input$setup_subset_mask)
      mask_intervals <- figure_out_mask(mask_input)
      
      if (!is.null(mask_intervals)) {
        dataset <- mask_(dataset, mask_intervals)
      }
      
      selected_rows <- isolate(input$setup_subset_select_rows)
      
      if (length(selected_rows) > 0) {
        dataset <- remove_selected_rows(dataset, selected_rows)
      }
      
      selected_attribs <- isolate(input$setup_subset_attrib_cols)
      
      if (length(selected_attribs) > 0) {
        attribute(dataset) <-
          remove_selected_attribs(dataset, selected_attribs)
      }
      
      output$setup_subset_attrib_preview <- DT::renderDataTable({
        attribute(dataset)
      })
      
      output$setup_subset_spectra_preview <- DT::renderDataTable({
        spectra_table <- spectra(dataset)
        rownames(spectra_table) <- hsdar::idSpeclib(dataset)
        colnames(spectra_table) <- hsdar::wavelength(dataset)
        spectra_table
      })
    })
  
  export_table <- reactive({
    if (input$export_vegindex_all_indices) {
      selected_indices <- available_indices
    } else {
      selected_indices <- input$export_table_vegindex_vi
    }
    
    if (input$export_by_attrib) {
      mean_by_attribute <- input$export_by_attrib_select
    } else {
      mean_by_attribute <- NULL
    }
    
    table <- get_export_table(
      data = data(),
      table_type = input$export_select_table_type,
      indices = selected_indices,
      by_attribute = mean_by_attribute
    )
    table
  })
  
  output$table_attributes <- DT::renderDataTable(data_attrib(),
                                                 options = list(scrollX = TRUE))
  output$table_spectra <- DT::renderDataTable(data_spectra(),
                                              server = TRUE,
                                              options = list(scrollX = TRUE))
  output$table_vegindex <- DT::renderDataTable(
    data_vegindex(),
    extensions = 'Buttons',
    options = list(
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      header = FALSE
    )
  )
  
  # TODO: Maybe these two can be merged?
  export_table_preview <- eventReactive(input$export_preview_table_button, {
      export_table()
    })
  
  output$export_table_preview <- DT::renderDataTable(export_table_preview(),
                                                     server = TRUE,
                                                     options = list(scrollX = TRUE))
  
  output$export_download_table <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(),
             "_specalyzer_",
             input$export_select_table_type,
             "_data.csv")
    },
    content = function(file) {
      write.csv(x = export_table(),
                file = file,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Menu updates ------------------------------------------------------------
  
  observe({
    veg_index_multiple_select_menus <- c("export_table_vegindex_vi", 
                                         "table_vegindex_select")
    veg_index_single_select_menus <- c("vi_index_select", "plot_field_index")
    sapply(veg_index_single_select_menus, function(menu) 
      updateSelectInput(session, menu, choices = available_indices, selected = "NDVI"))
    sapply(veg_index_multiple_select_menus, function(menu) 
      updateSelectInput(session, menu, choices = available_indices))
  })
  
  observe({
    updateSelectInput(session, "setup_subset_select_rows", choices = all_available_samples())
    updateSelectInput(session, "setup_subset_attrib_cols", choices = all_available_attributes())
  })
  
  observe({
    updateSelectInput(session, inputId = "plot_field_matrix", choices = available_field_matrices())  
  })
  
  observe({
    selectable_attribs <- available_attributes()
    select_attribute_menus <- c("export_by_attrib_select", 
                                "plot_field_attribute",
                                "plot_pca_size", 
                                "plot_pca_color", 
                                "spec_plot_attribute_select", 
                                "vi_plot_scatter_size", 
                                "vi_plot_attribute_select", 
                                "table_attribute_column_select")
    sapply(select_attribute_menus, function(menu) 
      updateSelectInput(session, menu, 
                        choices = c("Select an attribute" = "", selectable_attribs)))
  })
  
  observe({
    selectable_samples <- available_samples()
    select_sample_menus <- c("spec_plot_sample_select", 
                             "vi_plot_sample_select",
                             "table_attribute_row_select", 
                             "table_vegindex_row_select")
    sapply(select_sample_menus, function(menu) 
      updateSelectInput(session, menu, choices = selectable_samples))
  })
})
