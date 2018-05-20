# TODO: Remove this func
#' @export
plot_vegindex <- function(data, index, by_attribute = NULL, type = 'all', ...) {

  # Get a table of the index calculated for each sample:
  plot_data <- data.frame(
    id = idSpeclib(data) %>% as.character(),
    vi = specalyzer::vegindex(data, index)
  )

  if(!is.null(by_attribute)) {

    grp <- get_attr_column(data, by_attribute)
    plot_data <- data.frame(plot_data, grp)
    colnames(plot_data) <- c('id', index, by_attribute)

  }

  if(type == 'all') {

    p <- plot_vegindex_by_file(plot_data, by_attribute)

  } else if (type == 'boxplot') {
    p <- plot_vegindex_boxplot_1(plot_data, by_attribute, ...)
  } else if (type == 'scatter') {
    p <- plot_vegindex_scatter(plot_data, by_attribute, ...)
  } else {
    stop("Invalid plot type")
  }

  process_vegindex_plot(p, plot_data, by_attribute, type, index)

}

# TODO: Type out some comments here
process_vegindex_plot <- function(p, plot_data, by_attribute, type, index) {

  sample_plot_xaxis <- list(
    title = "Individual samples",
    showticklabels = FALSE,
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE
    )

  normal_xaxis <- list(
    title = by_attribute,
    zeroline = FALSE,
    showline = FALSE
  )

  xaxis <- if(type == 'all') sample_plot_xaxis else normal_xaxis

  plotly::layout(
    p,
    xaxis = xaxis,
    yaxis = list(title = index, zeroline = FALSE),
    margin = list(b = 70)
    )
}

#'@export
plot_vegindex_boxplot <- function(speclib_data, index, group_by_attribute,
                                  split_by_attribute = NULL, ...) {

  # TODO: Reimplement orientation. As of now broken. Plots come out looking
  # weird when orientation = h. Might have smth to do with boxmode = group?
  orientation = 'v'
  
  main_attribute_name <- group_by_attribute
  main_attribute_vector <- get_attr_column(speclib_data, main_attribute_name)
  index_vector <- specalyzer::vegindex(speclib_data, index) %>% unlist()
  split_attribute_vector <- NULL
  split_attribute_name <- NULL
  if(!is.null(split_by_attribute)) {
    split_attribute_vector <- get_attr_column(speclib_data, split_by_attribute)
    split_attribute_name <- split_by_attribute
  }
  if(orientation == 'v') {
    p <- plot_ly(y=index_vector,x=main_attribute_vector,
                 split = split_attribute_vector, type='box',...)
  } else if(orientation == 'h') {
    p <- plot_ly(x=index_vector,y=main_attribute_vector,
                 split = split_attribute_vector, type='box',...)
  } else {
    stop("Invalid boxplot orientation")
    }
  p %>% layout(boxmode = "group")
}

# TODO: Make away with this
plot_vegindex_boxplot_1 <- function(plot_data, ...) {
  p <- plotly::plot_ly() %>%
    plotly::add_boxplot(y = plot_data[,2], x = plot_data[,3],
                        split = as.character(plot_data[,3]), ...)
  p
}

plot_vegindex_scatter <- function(plot_data, by_attribute = NULL, ...) {

  # if(!is.null(by_attribute))

  p <- plotly::plot_ly() %>%
    plotly::add_markers(x = plot_data[,3], y = plot_data[,2], ...)
  p
}

plot_vegindex_by_file <- function(plot_data, by_attribute = NULL, ...) {

  colors <- NULL

  if(!is.null(by_attribute)) {
    colors <- plot_data[,3]

    if(is.integer(colors) | is.factor(colors)) {
      colors <- as.character(colors)
    }

  }

  index_name <- colnames(plot_data)[2]

  f <- function(i) {

    hover <- paste0("Sample: ", plot_data[i,1],"<br>",
                    index_name, ": ", round(plot_data[i,2], 2))

    if(!is.null(by_attribute)) {

      attrib_value <- plot_data[i,3]

      if(is.numeric(by_attribute) & !is.integer(by_attribute)) {
        attrib_value <- round(attrib_value, 4)
      }

      hover <- paste0(hover, "<br>", by_attribute, ": ", attrib_value)
    }

    hover
  }

  hovertext <- vapply(1:nrow(plot_data), FUN = f, FUN.VALUE = character(1))

  ids <- plot_data[,1] %>% as.character

  p <- plotly::plot_ly() %>% plotly::add_markers(x = ids, y = plot_data[,2],
                                                 color = colors, text = hovertext, ...)
  p
}
