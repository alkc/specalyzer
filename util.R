#' Return a subset of a speclib object matching a vector of sample ids.
#'
#' This function takes a speclib object and a vector of sample names as input
#' and extracts a subset of speclib rows that matches the supplied sample names.
#' @param data The queried speclib object (Speclib)
#' @param sample_name The target subset of samples (character)
#' @return A speclib object containing our target subset of samples.
#' @export
subset_speclib_by_sampleid <- function(data, sample_names) {
  
  all_samples <- idSpeclib(data)
  sample_names <- as.character(sample_names)
  
  f <- function(sample_name) {
    index <- which(idSpeclib(data) == sample_name)
    
    if(length(index) == 0) {
      NA
    } else {
      index
    }
    
  }
  
  row_nbrs <- vapply(sample_names, f, FUN.VALUE = integer(1))
  
  if(any(is.na(row_nbrs))) {
    
    missing <- which(is.na(row_nbrs))
    missing_samples <- paste(sample_names[missing], collapse = " ")
    row_nbrs <- na.omit(row_nbrs)
    sample_names <- sample_names[!missing]
    e <- paste0("Could not find: ", missing_samples, " in data. Ignoring.")
    warning(e)
  }
  
  subset <- data[row_nbrs,]
  idSpeclib(subset) <- sample_names
  subset
  
}

get_names_of_attribs <- function(data) {
  
  if(is.null(data)) return(NULL)
  
  attribute(data) %>% colnames()
}

figure_out_mask <- function(mask_intervals) {
  x <- mask_intervals %>% 
    strsplit(split = ",") %>% 
    unlist %>% 
    as.numeric
  
  if(any(is.na(x))) {
    NULL
  } else if(length(x) %% 2 == 0 && length(x) > 0) {
    x
  } else {
    NULL
  }
}

get_inverted_mask_interval <- function(data, mask_interval) {
  boundaries <- c(
    min = data %>% wavelength() %>% min,
    max = data %>% wavelength() %>% max
  )
  mask_interval <- c(300,400,1000,1200)
  # mask_interval <- mask_interval[1:length(mask_interval) %% 2 == 0]
  mask_interval <- mask_interval[-c(1,length(mask_interval))]
  mask_interval <- mask_interval + 1
}

get_selected_attribs <- function(data, attributes) {
  
  if(is.null(data)) return(NULL)
  
  hsdar::attribute(data) %>% 
    select_(.dots = attributes)
}

remove_selected_attribs <- function(data, attributes) {
  
  attributes <- vapply(attributes, function(x) paste0("-",x), 
                       FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  hsdar::attribute(data) %>% 
    select_(.dots = attributes)
}

remove_selected_rows <- function(data, sample_ids) {
  
  kill_list <- !hsdar::idSpeclib(data) %in% sample_ids
  new_ids <- hsdar::idSpeclib(data)[kill_list]
  data <- data[kill_list]
  idSpeclib(data) <- new_ids
  data
}

get_plot_subset <- function(data, selected_attrib, values, is_range) {
  
  attribs <- hsdar::attribute(data)
  sample_ids <- hsdar::idSpeclib(data)
  
  if(is_range) {
    
    if(length(values) != 2) {
      stop("'values' should specify an interval")
    }
    
    lwr_bound <- values[1]
    upr_bound <- values[2]
    
    f <- function(x) lwr_bound <= x & x <= upr_bound
    
    subset <- which(f(attribs[,selected_attrib]))
    
  } else if(!is_range) {
    
    f <- function(x) x %in% values
    
    subset <- which(f(attribs[,selected_attrib]))
    
  } else {
    stop("is_range should equal TRUE or FALSE")
  }
  
  subset_data <- data[subset,]
  idSpeclib(subset_data) <- sample_ids[subset]
  subset_data
  
}


#' Check if spectral data sample ids match attribute data filename rows
#'
#' @param spectral_data A speclib object (Speclib)
#' @param attribute_data A table with attribute data (data.frame)
#'
#' This function checks id all the sample ids in the Speclib object have a 
#' corresponding row in the attribute data table, by checking the filename
#' column in the attribute data table.
#'
#' @return boolean
#' @export
spectral_data_matches_attribute_data <- function(spectral_data, attribute_data) {
  
  spectral_files <- idSpeclib(spectral_data)
  attrib_filenames <- attribute_data[['filename']]
  
  if(length(spectral_files) != length(attrib_filenames)) {
    stop("Unequal amount of rows in spectral and attribute data")
  }

  # Check if filenames match
  all(spectral_files %in% attrib_filenames) && all(attrib_filenames %in% spectral_files)
  
}

#' Sort attribute data using ids of speclib object as target order
#'
#' This function takes a table containing attribute data as input, and sorts
#' the table on the filename column using the sample ids stored in a speclib
#' object as a target order. The function assumes that the speclib object and
#' attribute data table have an equal number of rows, and that the filenames in
#' the attribute data filename column and the filenames stored in the speclib
#' object ID attribute are identical.
#' 
#' @param attribute_data The attribute data table to be sorted (data.frame)
#' @param spectral_data A Speclib object of corresponding spectral samples (Speclib)
#' @return Sorted attribute data table
#' @export
sort_attribute_data <- function(attribute_data, spectral_data) {
  # If the order of the attribute data rows matches the order of the
  # spectral data, then no sorting is required:
  if(all(idSpeclib(spectral_data) == attribute_data[['filename']])) {
    return(attribute_data)
  }
  # If not, reorder the rows of the attribute data table to match the
  # order of samples in the speclib object 
  target_order <- idSpeclib(spectral_data)
  new_order <- match(target_order, attribute_data[['filename']] %>% unlist)
  attribute_data[new_order,]
}