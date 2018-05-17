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
    # TODO: Add stackoverflow credit here
    # This removes the x-axis title and tells plotly to sort the x-axis on
    # the correlation coefficient value.
    xform <- list(categoryorder = "array",
                  categoryarray = plot_data$correlation_coefficient,
                  tickangle = 45,
                  title = "")
  } else if(attribute_type == "categorical") {
    plot_data <- get_significant_vegindices(vi_table, attribute_vector)
  }
  p <- plotly::layout(p,xaxis = xform)
  p
}

get_vegindex_attribute_correlations <- function(vi_table,attribute_vector, corr_method = "pearson") {
  plot_data <- data.frame(
    index = colnames(vi_table),
    correlation_coefficient = rep(NA, ncol(vi_table))
  )
  for(i in seq(ncol(vi_table))) {
    plot_data[i,2] <- cor(vi_table[,i], attribute_vector, method = corr_method)
  }
  plot_data
}
