# For tab-separated long form data
process_long_data <- function(file_path) {
  long_data <- read.table(file_path, header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)
  long_data <- data.frame(t(long_data))
  data <- data.frame(long_data[2,])
  colnames(data) <- long_data[1,]
  data
}

# For tab-separated wide form data
process_wide_data <- function(file_path) {
  read.table(file_path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
}

process_spectrawiz_data <- function(file_path) {
  # Skip = 2 skips the two lines of metadata when reading the table.
  # Perhaps add some handling here in case that meta data is missing
  # Though in that case the user should specify they want long type files
  long_data <- read.table(
    file_path,
    header = FALSE,
    stringsAsFactors = FALSE,
    skip = 2,
    check.names = FALSE
    )

  # Transpose the wavelengths from rows to columns
  data <- as.data.frame(t(long_data[,-1]), stringsAsFactors = FALSE)
  colnames(data) <- c(long_data[,1])

  # Remove path from filename
  file_name <- basename(file_path)
  row.names(data) <- c(file_name)
  data
}

# For field spec readers
# TODO: test this thing
process_asd_data <- function(file_path) {

  # TODO: Work out how to let user specify if they want
  # raw spectra, reflectance, or any of the other options
  data <- asdreader::get_spectra(file_path)

}
