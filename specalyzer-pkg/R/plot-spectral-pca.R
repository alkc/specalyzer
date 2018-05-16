#' @export
plot_spectral_pca <- function(prcomp_object, data = NULL, color = NULL, size = NULL,...) {

  xaxis_title <- prcomp_object %>% get_pca_plot_axis_title(1)
  yaxis_title <- prcomp_object %>% get_pca_plot_axis_title(2)

  plot_data <- data.frame(
    id = prcomp_object[[5]] %>% row.names(),
    PC1 = prcomp_object %>% get_PC(1),
    PC2 = prcomp_object %>% get_PC(2)
  )

  color_formula <- NULL
  size_formula <- NULL

  if(!is.null(color)) {
    color_col = get_attr_column(data, color) %>% as.factor
    color_formula <- paste0("~" , color) %>% as.formula
    # hover_attr_1_name <- paste0(color, ":")
    plot_data <- data.frame(color_col, plot_data)
    names(plot_data)[1] <- color
  }

  if(!is.null(size)) {
    size_formula <- paste0("~" , size) %>% as.formula
    size_col = get_attr_column(data, size)
    # hover_attr_2_name <- paste0(size, ":")
    plot_data <- data.frame(size_col, plot_data)
    names(plot_data)[1] <- size
  }

  row_sequence <- 1:nrow(plot_data)
  plot_data <- data.frame(row_sequence, plot_data)
  text_formula <- get_text_formula(color, size)


  p <- plot_data %>%
    plotly::plot_ly(x = ~PC1,
                    y = ~PC2,
                    text = text_formula,
                    color = color_formula,
                    size = size_formula,
                    type = 'scatter',
                    mode = 'markers',
                    hoverinfo = "text+color+x+y", ...)


    p %>% plotly::layout(
      xaxis = list(title = xaxis_title, showline = FALSE,
                   zeroline = FALSE, showgrid = TRUE),
      yaxis = list(title = yaxis_title, showline = FALSE,
                   zeroline = FALSE, showgrid = TRUE))

}

get_text_formula <- function(color, size) {

  text <- '"data row:", row_sequence, "|", "id:", id'

  if(!is.null(color)) {
    text <- paste0(text, ', "|", "', color, ':" ,', color)
  }

  if(!is.null(size)) {
    text <- paste0(text, ', "|", "', size, ':" ,', size)
  }

  as.formula(paste0("~paste(", text,")"))

}

# Helper functions for dealing with prcomp objects


get_pca_plot_axis_title <- function(prcomp_object, pc_nbr) {
  var_explained <- prcomp_object %>% get_variance_explained_by_pc(pc_nbr) %>%
    (function(x) x * 100) %>% round(2)

  PC_name <- paste0("PC", pc_nbr)
  paste0(PC_name," (", var_explained, "% explained var.)")

}

get_variance_explained_by_pc <- function(prcomp_object, pc_nbr) {

  sdev <- unlist(prcomp_object["sdev"])
  sdev[pc_nbr]^2 / sum(sdev^2)

}

get_PC <- function(prcomp_object, pc_nbr) {
  prcomp_object[["x"]][,pc_nbr]
}
