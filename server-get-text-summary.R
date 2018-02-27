get_text_summary <- function(data) {
  spec_desc <- get_spectral_data_description(data)
  attr_desc <- get_attribute_data_description(data)
  output <- tagList(h1("Success!"))
  directions <- tagList(
    h3("Directions"),
    p(
      "Use the tab-menu above to to access the different functions made available by specalyzer.",
      "The", strong("Setup"), "menu allows you to subset your data into a working dataset",
      "The", strong("Visualize"), "menu contains a variety of plotting functions for visualizing the spectra or vegetation indices",
      strong("Export"), "allows you to export your merged working data set, or a table of selected vegetation indices calculated from your data."
    ))
  output <- tagAppendChildren(output, spec_desc, attr_desc, directions)
  output
}

get_attribute_data_description <- function(data) {
  if(length(hsdar::attribute(data)) == 0) {
    output <- p("No attribute data was detected in the sample") 
  } else {
  attrib_data <- hsdar::attribute(data)
  attrib_names <- colnames(attrib_data)
  nbr_attribs  <- length(attrib_names)
  output <- p("The uploaded attribute data contains ", 
    strong(nbr_attribs), " variables: ",
    strong(paste0(attrib_names, collapse = ", ")), 
    "."
  )}
  output
}

get_spectral_data_description <- function(data) {
  wavelengths <- hsdar::wavelength(data)
  wvl_range <- paste(min(wavelengths),"-", max(wavelengths))
  nbr_bands <- length(wavelengths)
  nbr_samples <- nrow(data)
  p("Specalyzer detected ", strong(nbr_samples), " spectral samples in the uploaded dataset, containing:",
    strong(nbr_bands), " bands in the wavelength range of ", strong(wvl_range, "nm"))
}