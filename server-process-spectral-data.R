process_spectral_data <- function(input_files, file_format, project_id) {
  # Take list of spectral files and read it via plantspec
  # print(input_files[['datapath']])
  
  file_names <- unlist(input_files[['name']])
  spectral_data <- specalyzer::read_spectral_data(
    spectral_files = input_files[['datapath']],
    file_format = file_format,
    row_names = file_names)
  spectral_data
}

# TODO: Sort this one out.
process_attribute_data <- function(input_file, spectral_data) {
  
  # This function reads the attribute data and pokes it a bit to see if the rows
  # in the table correspond to the rows in the speclib object. For now this 
  # function will throw errors if the filenames in the required filename 
  # column in the attrib data do not correspond to the filename stored in the 
  # speclib object. Likewise it'll throw errors if the nbr of rows differs 
  # between the attribute and spectral data

  # Read attrib table
  attrib_path <- input_file[['datapath']]
  # print(attrib_path)
  attrib_data <- read_delim(attrib_path, delim = "\t") %>% 
    as.data.frame(stringsAsFactors = FALSE)
  
  # Require a column named 'filename' in the attrib table
  if(!"filename" %in% colnames(attrib_data)) {
    shiny::showNotification("'filename' column is missing from attribute table", type = "error")
    return(spectral_data)
  }
  
  # Throw an error if the filename column contains non-unique entries
  if(any(table(attrib_data[['filename']]) > 1)) {
    shiny::showNotification("filenames in the 'filename' columns must be unique", type = "error")
    return(spectral_data)
  }
  
  # Check if filenames match between attrib and spectral data
  if(!spectral_data_matches_attribute_data(spectral_data, attrib_data)) {
    shiny::showNotification("Filenames in the attribute table must match the filenames of the spectral files", type = "error")
    return(spectral_data)
  }
  
  # Sort attrib table rows using the ordering of files in spectral_data as template
  attrib_data <- sort_attribute_data(attrib_data, spectral_data)
  
  # Move filename column to row names as it is no longer needed

  rownames(attrib_data) <- attrib_data %>% pull(filename) %>% tools::file_path_sans_ext()
  # idSpeclib(spectral_data) <- idSpeclib(spectral_data) %>% tools::file_path_sans_ext()
  hsdar::attribute(spectral_data) <- attrib_data[,-1]
  spectral_data
}