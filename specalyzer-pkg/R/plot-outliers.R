#' @export
get_outlier_plot <- function(speclib_data, sd_threshold = 3, ...) {
  prcomp_object <- prcomp(spectra(speclib_data), ...)
  plot_data <- data.frame(
    sample_ids = as.character(idSpeclib(spectral_data)),
    PC1 = prcomp_object$x[,1],
    PC2 = prcomp_object$x[,2],
    is_outlier = rep(NA, nrow(prcomp_object$x))
    )
  max_sd <- prcomp_object$sdev[c(1,2)] * sd_threshold
  plot_data$is_outlier <- (abs(plot_data$PC1) > max_sd[1] | abs(plot_data$PC2) > max_sd[2])
  # Credit goes to https://stackoverflow.com/a/43292865
  # for legend-title:
  legendtitle <- list(yref='paper',xref="paper",y=1.05,x=1.1,
                      text="Outlier",showarrow=FALSE)
  plot_ly(
    plot_data,
    x=~PC1, y=~PC2,
    text=~paste("sample", sample_ids),
    split=~is_outlier,
    mode="markers",
    type="scatter"
  ) %>% layout(annotations = legendtitle)
}
