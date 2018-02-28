# filehandling funcs

#' @export
read_spectral_data <- function(spectral_files, file_format = NULL, row_names = NULL,
                               output_type = 'speclib') {

    file_check <- file.exists(spectral_files)

    # Throw error if any of the supplied paths is missing
    if (!all(file_check)) {
      missing_files <- spectral_files[!file_check]
      missing_files <- paste(missing_files, collapse = "\n")
      err_msg <-
        paste("The following files to not exist:\n", missing_files)
      stop(err_msg)
    }

    # TODO: Add some code to try to guess the file format from file ending
    # if (is.null(file_format) || !file_format_supported(file_format)) {
    #   stop("Please specify a valid file format")
    # }


    # Check if length user supplied custom row_names vector matches the length
    # of supplied spectral files to merge
    if (!is.null(row_names) && length(row_names) != length(spectral_files)) {

      err_msg <- paste(
        "Length of row_names:",
        length(row_names),
        "does not match the length of the data rows:",
        length(spectral_files)
      )

      stop(err_msg)
    }

    # if(read_from_directory && is.null(file_pattern)) {
    #   file_pattern <- infer_file_pattern(file_format)
    #   if(is.null(file_pattern)) {
    #     stop("Could not infer a valid file pattern")
    #   }
    # }
    # if(read_from_directory) {
    #   file_list <- list.files(path, file_pattern, full.names = TRUE)
    # } else {
    #   file_list <- path
    # }

    spectral_data <- read_from_file_list(spectral_files, file_format)
    spectral_data <- cleanup_spectral_file_ids(spectral_data, spectral_files)
    spectral_data <- as.matrix(spectral_data)

    if (output_type == 'matrix') {
      return(spectral_data)
    }

    # TODO: Merge all code after this comment into own function
    spectral_data <- hsdar::speclib(spectral_data, as.numeric(colnames(spectral_data)))

    if(!is.null(row_names)) {
      hsdar::idSpeclib(spectral_data) <- row_names
    }
    spectral_data

  }

cleanup_spectral_file_ids <- function(spectral_data, file_list) {

  # Extract basenames from file list to use as row names in spectral matrix
  # TODO: Refactor the code so that this part is skipped if user supplied
  # their own row names.
  spectral_ids <- vapply(file_list,
                         function(x)
                           tools::file_path_sans_ext(basename(x)),
                         USE.NAMES = FALSE, FUN.VALUE = character(1))

  # TODO: Remove the need to pull data out of a list if there is only one row
  # of data. Preferably do the task somewhere upstreams.

  if (length(spectral_data) == 1) {
    spectral_data <- spectral_data[[1]]
  }
  row.names(spectral_data) <- spectral_ids
  spectral_data
}

read_from_file_list <- function(file_list, file_format)  {

  # Get correct file processing function. That is a function that reads an
  # individual spectral file and outputs a row of spectral data
  file_processing_function <- get_processing_function(file_format)

  # Determine how to parse the input.
  data <- lapply(file_list, file_processing_function)

  # Multiple datasets are reduced to one data.frame using rbind
  if (length(data) > 1) {
    data <- dplyr::bind_rows(data)
  }
  data
}

get_processing_function <- function(spectral_data_format) {

  process_funcs <- c("long" = process_long_data,
                    "wide" = spectral_data_format,
                    "spectrawiz" = process_spectrawiz_data,
                    "asd" = process_asd_data)

  if (!spectral_data_format %in% names(process_funcs)) {
    err_msg <- paste(
      "Unsupported spectral file format:",
      spectral_data_format
    )
    stop(err_msg)
  }

  process_funcs[[spectral_data_format]]

}
