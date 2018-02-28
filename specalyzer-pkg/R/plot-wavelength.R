
plot_wavelength <- function(data, wavelengths, by_attribute = NULL, type = 'auto') {

  stop("fix me")

  if(length(wavelengths) > 1 && !is.null(by_attribute)) {
    wavelengths <- wavelengths[1]
  }

  plot_data <- get_wavelengths(data, wavelengths) %>% as.data.frame
  plot_data <- cbind(get_attr_column(data, by_attribute), plot_data)
  colnames(plot_data) <- c(by_attribute, wavelengths)

  melted_plot_data <- reshape2::melt(plot_data, id.vars = c("yr"),
                                     value.name = "Reflectance",
                                     variable.name = "Wavelength")




}
