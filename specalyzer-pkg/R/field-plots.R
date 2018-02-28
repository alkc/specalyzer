#' @export
plot_vegindex_matrix <- function(data, veg_index, field_layout_matrix) {
    veg_index_vals <- vegindex(data, veg_index)
    plot <-
      get_field_plot(field_layout_matrix, veg_index_vals, data, veg_index)
    plot
  }

#' @export
plot_attribute_matrix <- function(data, attribute, field_layout_matrix) {
    attribute_values <- get_attr_column(data, attribute)
    plot <-
      get_field_plot(field_layout_matrix, attribute_values, data, attribute)
    plot
  }

order_z_vals <- function(z_values, speclib, field_layout_matrix) {
  z_values <- unlist(z_values)
  # TODO: Order the z_values from idSpeclib(data) to unlist(layout).
  desired_order <- as.vector(field_layout_matrix)
  names(z_values) <- hsdar::idSpeclib(speclib)
  z_values <- z_values[desired_order]
  z_values
}

get_field_plot <- function(field_layout_matrix, z_values,
                           speclib, z_value_label) {
    validation_result <- field_matrix_is_valid(field_layout_matrix, speclib)

    if (!validation_result) {
      stop(
        "One or more sample ids provided in field layout matrix could not be found in spectral dataset"
      )
    }

    z_values <- order_z_vals(z_values, speclib, field_layout_matrix)
    field_layout_dims <- dim(field_layout_matrix)
    plot_matrix <-
      matrix(z_values, field_layout_dims[1], field_layout_dims[2])
    hover_text <-
      build_heatmap_hover_text(field_layout_matrix, plot_matrix, z_value_label)
    # plotly::plot_ly(z = plot_matrix, type = "heatmap", colors = "Spectral", showlegend = TRUE)
    plotly::plot_ly(
      z = plot_matrix,
      type = "heatmap",
      colors = "Spectral",
      text = hover_text,
      hoverinfo = "text",
      showlegend = TRUE
    ) %>%
      get_formatted_heatmap(z_value_label)
  }

get_annotations <- function(field_layout_matrix, plot_matrix, z_value_label) {
    annotation_nbr <- length(field_layout_matrix)
    annotations <- rep(NA, times = annotation_nbr) %>% as.list()
    for (cell_index in 1:annotation_nbr) {
      curr_sample <- field_layout_matrix[cell_index] %>% as.character
      curr_z_value <- plot_matrix[cell_index] %>% as.character
      annotations[cell_index] <- list(
        x = curr_sample,
        y = curr_z_value,
        font = list(color = "#FFFFFF"),
        showarrow = FALSE,
        text = curr_sample,
        xref = "x",
        yref = "y"
      )
    }
  }

get_formatted_heatmap <- function(field_matrix_plot, z_value_label) {
    field_matrix_plot %>%
      plotly::layout(
        title = paste0(z_value_label, " by sample"),
        yaxis = list(
          autorange = "reversed",
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE,
          ticks = ""
        ),
        xaxis = list(
          zeroline = FALSE,
          showline = FALSE,
          showticklabels = FALSE,
          showgrid = FALSE,
          ticks = ""
        )
      )
  }

build_heatmap_hover_text <- function(field_layout_matrix,
           plot_matrix,
           z_value_label) {
    get_hovertext <- function(x) {
      z_value <- round(plot_matrix[x], digits = 3)
      paste0("id: ",
             field_layout_matrix[x],
             "<br>",
             z_value_label,
             ": ",
             z_value)
    }
    dims <- dim(field_layout_matrix)
    hover_data <- matrix(NA, dims[1], dims[2])
    hover_data[] <-
      vapply(
        1:length(field_layout_matrix),
        FUN = get_hovertext,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )
    hover_data
  }

field_matrix_is_valid <- function(field_layout_matrix, speclib) {
  sample_ids <- hsdar::idSpeclib(speclib)
  sample_ids_in_matrix <- unlist(field_layout_matrix)
  all(
    vapply(sample_ids_in_matrix, function(x)
      x %in% sample_ids,
      FUN.VALUE = logical(1), USE.NAMES = FALSE)
  )
}
