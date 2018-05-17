get_spectral_plot <- function(dataset, input, output) {
  
    spectral_plot_data <- dataset
    
    type <- input$spec_plot_type
    sample_select <- input$spec_plot_sample_select
    attrib_select <- input$spec_plot_attribute_select
    # mask_intervals <- figure_out_mask(input$spec_plot_mask)
    selected_plot_type <- input$spec_plot_by
    
    if(selected_plot_type == 'samples' && length(sample_select) > 0) {
      spectral_plot_data <- subset_speclib_by_sampleid(spectral_plot_data, sample_select)
    } else {
      spectral_plot_data <- dataset
    }
    
    if(selected_plot_type == "attributes" && length(attrib_select) > 0) {
      byattributes <- attrib_select[1]
    } else {
      byattributes <- NULL
    }
    
    if(input$spec_plot_type == "individual") {
      
      if(length(sample_select) > 10 || length(sample_select) < 1) {
        stop("Please select between 1 - 10 samples")
      }
      
      type <- "mean"
      individual <- TRUE 
    } else {
      individual <- FALSE
    }
    
    # if(!is.null(mask_intervals)) {
    #   spectral_plot_data <- mask_(spectral_plot_data, mask_intervals)
    # }
    
    specalyzer::plot_spectrum(
      spectral_plot_data, 
      aggregate_by = byattributes,
      type = type, 
      plot_individual_spectra = individual) %>% 
      plotly::layout(
        title = input$spec_plot_title,
        xaxis = list(title = input$spec_plot_xlab), 
        yaxis = list(title = input$spec_plot_ylab))
}

get_vi_plot <- function(dataset, input, output) {
  # Process all inputs
  
  # Figure out if user wants custom or built-in index
  if (input$vi_plot_custom_index == TRUE) {
    vi <- input$vi_custom_index_input
  } else {
    vi <- input$vi_index_select
  }
  
  # TODO: Fix this up:
  # If call to vegindex returns error or a dataset of all NAs, then tell user
  # their data does not support the calculating of selected vegetation index
  validate(
    need(
      all(is.na(try(specalyzer::vegindex(dataset, vi), silent = TRUE))) == FALSE,
      paste('Your data does not contain reflectance at the neccessary wavelengths to calculate:', vi))
    )
  
  if (input$vi_plot_scatter_size != "") {
    by_size <- get_attr_column(dataset, input$vi_plot_scatter_size)
  } else {
    by_size <- NULL
  }
  
  showlegend <- if(!is.null(by_size)) TRUE else FALSE
    # selected_plot_type <- input$vi_plot_by
  byattribute <- if(input$vi_plot_attribute_select != "") input$vi_plot_attribute_select else NULL
  type <- input$vi_attrib_plot_type
  boxpoints <- input$vi_plot_boxplot_boxpoints
  # print(boxpoints)
  
  if (boxpoints == "all") {
    jitter <- 0.3
  } else if (boxpoints == "none") {
    boxpoints <- FALSE
    jitter <- 0
  } else {
    jitter <- 0
  }
  
  if (input$vi_attrib_plot_type == 'boxplot') {
    p <- specalyzer::plot_vegindex(
      dataset,
      index = vi,
      by = byattribute,
      type = "boxplot",
      boxpoints = boxpoints,
      jitter = jitter
    )
  } else if (input$vi_attrib_plot_type == 'scatter') {
    p <- specalyzer::plot_vegindex(
      dataset,
      index = vi,
      by = byattribute,
      type = 'scatter',
      color = by_size
    )
    p <- plotly::layout(p, showlegend = showlegend)
  } else {
    p <- specalyzer::plot_vegindex(dataset,
                                  index = vi,
                                  by = byattribute,
                                  type = 'all')
    p <-
      plotly::layout(p,
                     showlegend = showlegend,
                     legend = list(orientation = 'h'))
  }
  
  p <- plotly::layout(p, title = input$vi_plot_title)
  p
}

get_field_matrix_plot <- function(dataset, datapath, input, output) {
  
  chosen_matrix <- input$plot_field_matrix
  if(is.null(chosen_matrix)) {
    stop("No field matrices available.")
  }
  matrix_path <- chosen_matrix
  m <- read.csv(matrix_path, header = FALSE, stringsAsFactors = FALSE) %>% as.matrix()
  # print(m)
  # dataset %>% idSpeclib %>% print
  if(input$plot_field_type == "vegindex") {
    index <- input$plot_field_index
    if(input$plot_field_custom) {
      index <- input$plot_field_custom_index
    }
    p <- get_vegindex_field_plot(dataset, index, m)
  } else if (input$plot_field_type == "attribute"){
    attribute <- input$plot_field_attribute
    p <- get_attribute_field_plot(dataset, attribute, m)
  } else {
    stop()
  }
  
  p 
  
}

get_vegindex_field_plot <- function(dataset, index, field_layout_matrix) {
  specalyzer::plot_vegindex_matrix(dataset, index, field_layout_matrix)
}

get_attribute_field_plot <- function(dataset, attribute, field_layout_matrix) {
  specalyzer::plot_attribute_matrix(dataset, attribute, field_layout_matrix)
}

get_pca_plot <- function(dataset, input, output) {
  color <- if(input$plot_pca_color != "") input$plot_pca_color else NULL
  size <-  if(input$plot_pca_size != "") input$plot_pca_size else NULL
  spec_pca <- get_spectral_pca(dataset, scale. = input$plot_pca_scale, 
                               center = input$plot_pca_center)
  plot_spectral_pca(spec_pca, data = dataset, color = color, size = size) %>% 
    plotly::layout(title = input$plot_pca_title)
}

get_outlier_plot <- function(dataset, input, output) {
  plot_outliers(dataset, 
                sd_threshold = input$plot_pca_outlier_sd,
                scale. = input$plot_pca_scale, 
                center = input$plot_pca_center) %>% 
    plotly::layout(title = input$plot_pca_title)
}

get_vegindex_selection_plot <- function(dataset, input,output) {
  p <- plot_vegindex_selection(dataset, 
                               attribute = input$viselection_attr_select,
                               attribute_type = input$viselection_attr_type)
  p
}