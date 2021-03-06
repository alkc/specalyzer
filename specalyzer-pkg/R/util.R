# Function for extracting specific attribute column from speclib object
#' @export
get_attr_column <- function(speclib_data, attribute) {
  hsdar::attribute(speclib_data) %>% dplyr::pull(.data[[attribute]])
}

# Determine if an attribute is continuous
# TODO: Check if this actually works.
is_attrib_continuous <- function(data, attribute) {
  attr <- get_attr_column(data, attribute)
  is.numeric(attr) && !is.integer(attr)
}

# prcomp() will complain if data passed to it contains columns with zero variance
# this function identifies those columns and removes them from the dataset
preprocess_spectra_for_pca <- function(spectral_matrix, wavelength) {
  keepers <- apply(spectral_matrix, 2, function(x) var(x) != 0)
  if(any(keepers == FALSE)) {
    removed_bands <- paste(wavelength[!keepers], collapse = ", ")
    msg <- paste0("Following bands were found to show no variance and were removed: ", removed_bands)
    message(msg)
  }
  spectral_matrix[,keepers]
}

#' @export
get_spectral_pca <- function(data, ...) {
  # Matrix has to be converted to df, otherwise prcomp won't catch
  # the rownames, for some reason
  spectral_matrix <- hsdar::spectra(data) %>% as.data.frame
  colnames(spectral_matrix) <- hsdar::wavelength(data)
  rownames(spectral_matrix) <- hsdar::idSpeclib(data)
  spectral_pca <- preprocess_spectra_for_pca(spectral_matrix, hsdar::wavelength(data)) %>% prcomp(...)
  spectral_pca
}

#' @export
get_wavelengths <- function(data, wavelengths) {
  available_wavelengths <- wavelength(data)
  spectra <- hsdar::spectra(data)
  output <- NULL
  get_wvl_index <- function(wvl) {
    which(available_wavelengths == wvl)
  }
  for(wvl in wavelengths) {
    i <- get_wvl_index(wvl)
    output <- cbind(output, spectra[,i])
  }
  colnames(output) <- wavelengths
  rownames(output) <- idSpeclib(data)
  output
}

get_masked_spectra <- function(data) {
  dropped_bands <- attr(data, "dropped")
  spectral_data <- spectra(data) %>% as.data.frame()
  colnames(spectral_data) <- wavelength(data)
  row.names(spectral_data) <- idSpeclib(data)
  if(is.null(dropped_bands)) {
    return(spectral_data)
  }
  boundaries <- wavelength(data) %>% (function(x) c(min(x), max(x)))
  is_within_boundaries <- function(i) {
    lower_boundary <- dropped_bands[i,1]
    upper_boundary <- dropped_bands[i,2]
    !(upper_boundary <= boundaries[1] | lower_boundary + 1 > boundaries[2])
  }
  within_bounds <- vapply(1:nrow(dropped_bands), is_within_boundaries,
                          FUN.VALUE = logical(1))
  if(!any(within_bounds)) {
    return(spectral_data)
  }
  dropped_bands <- dropped_bands[within_bounds,]
  NA_cols <- list()
  names <- rep(NA, nrow(dropped_bands))
  nbr_rows <- nrow(spectral_data)
  for(i in 1:nrow(dropped_bands)) {
    NA_cols[[i]] <- rep(NA, nbr_rows)
    names[i] <- apply(dropped_bands[i,], 1, mean)
  }
  if(nbr_rows > 1) {
    NA_cols <- cbind(sapply(NA_cols, function(x) x)) %>% as.matrix
    colnames(NA_cols) <- names %>% as.numeric()
  } else {
    NA_cols <- rep(NA, nrow(dropped_bands)) %>% t() %>% as.data.frame()
    colnames(NA_cols) <- names %>% as.numeric()
  }
  if(nbr_rows == 1) {
    restored_name <- data %>% attribute() %>% row.names()
    if(!length(restored_name) > 0) {}
     restored_name <- 1
    rownames(spectral_data) <- restored_name
  }
  spectral_data <- cbind(spectral_data, NA_cols)
  spectral_data
}

get_all_NA_cols <- function(table) {
  # Make bool. vector that lists columns in table only containing NAs as "FALSE"
  columns_to_keep <- apply(table, 2, function(x) all(is.na(x)))
  columns_to_keep <- unlist(columns_to_keep)
  columns_to_keep
}

calculate_all_vegindexes <- function(speclib_data) {
  specalyzer::vegindex(speclib_data, specalyzer::vegindex())
}

speclib_to_long_df <- function(speclib_data, attributes_to_group_by=NULL) {
  wavelengths <- as.character(wavelength(speclib_data) )
  extracted_spectra <- spectra(speclib_data)
  extracted_spectra <- as.data.frame(extracted_spectra, stringsAsFactors = FALSE)
  colnames(extracted_spectra) <- wavelengths
  
  if(is.null(attributes_to_group_by)) {
    long_spectra <- melt(extracted_spectra, 
                         direction = "long", 
                         variable.name = "wavelength", 
                         value.name = "reflectance")
  } else {
    attribute_data <- attribute(speclib_data)
    for(attribute in attributes_to_group_by) {
      attribute_vector <- attribute_data[[attribute]]
      extracted_spectra[[attribute]] <- attribute_vector
    }
    long_spectra <- melt(extracted_spectra, 
                         value.name = "reflectance", 
                         id.vars = attributes_to_group_by, 
                         variable.name = "wavelength") 
  }
  #Convert from probably factor:
  long_spectra$wavelength <- as.numeric(as.character(long_spectra$wavelength))
  # Return:
  long_spectra
  
}
