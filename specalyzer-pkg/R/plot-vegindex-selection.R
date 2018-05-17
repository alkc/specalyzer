#' @export
plot_vegindex_selection <- function(speclib_data, attribute,
                                    attribute_type = c("continuous", "categorical")) {

  # TODO: Add code for checking inputs
  attribute_name <- attribute
  attribute_vector <- get_attr_column(speclib_data, attribute)
  vi_table <- calculate_all_vegindexes(speclib_data)
  # Remove all only-NAs columns from vegindex table:
  # TODO: Add warning when index columns removed.
  vi_table <- vi_table[,!get_all_NA_cols(vi_table)]
  available_indices <- colnames(vi_table)
  # If response variable is continuous, then calculate the correlation
  # coefficient between each available index (supported by data) and the
  # attribute of interest, and visualize as a barplot.
  if(attribute_type == "continuous") {
    # TODO: Isolate into own func?
    plot_data <- get_vegindex_attribute_correlations(vi_table, attribute_vector)
    plot_data <- plot_data[order(plot_data$correlation_coefficient),]
    p <- plot_ly(plot_data,x=~index, y=~correlation_coefficient)
    p <- layout(p,yaxis=list(range=c(-1,1)))
    # Credit to @mtoto: https://stackoverflow.com/a/40703117
    # This removes the x-axis title and tells plotly to sort the x-axis on
    # the correlation coefficient value.
    xform <- list(categoryorder = "array",
                  categoryarray = plot_data$correlation_coefficient,
                  tickangle = 45,
                  title = "")
  } else if(attribute_type == "categorical") {
    plot_data <- get_significant_indices(vi_table, as.factor(attribute_vector))
    plot_data <- plot_data[order(plot_data$log_ten_p),]
    p <- plot_ly(plot_data, x=~index, y=~log_ten_p)
    xform <- list(categoryorder = "array",
                  categoryarray = plot_data$log_ten_p,
                  tickangle = 45,
                  title = "")
  }
  plotly::layout(p, xaxis = xform)
}

get_vegindex_attribute_correlations <- function(vi_table,attribute_vector, corr_method = "pearson") {
  plot_data <- data.frame(
    index = colnames(vi_table),
    correlation_coefficient = rep(NA, ncol(vi_table))
  )
  nbr_indices <- ncol(vi_table)
  for(i in seq(nbr_indices)) {
    plot_data[i,2] <- cor(vi_table[,i], attribute_vector, method = corr_method)
  }
  plot_data
}

get_significant_indices <- function(vi_table, attribute_vector) {
  nbr_indices <- ncol(vi_table)
  plot_data <- data.frame(index = colnames(vi_table),
                          p_value = rep(NA, nbr_indices),
                          log_ten_p = rep(NA, nbr_indices))
  for(i in seq(nbr_indices)) {
    predictor_index <- vi_table[,i]
    linear_model <- lm(predictor_index ~ attribute_vector)
    linear_model <- anova(linear_model)
    p_value <- linear_model[["Pr(>F)"]][1]
    plot_data[i,2] <- p_value
    plot_data[i,3] <- -log10(p_value)
  }
  plot_data
}
