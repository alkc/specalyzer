process_fieldmap_data <- function(input_files, speclib) {
  file_names <- unlist(input_files[['name']])
  fieldmaps <- input_files[['datapath']]
  validation_results <- validate_uploaded_maps(fieldmaps, speclib)
  # TODO: Write better error message
  if(!all(validation_results)) {
    exit("One or more field maps is wrong.")
  }
  # save_fieldmaps_to_proj_dir(fieldmaps, proj_name)
  basename(fieldmaps)
}

is_valid_fieldmap <- function(fieldmap_path, speclib) {
  sample_names <- idSpeclib(speclib)
  fieldmap <- load_fieldmap(fieldmap_path)
  all(fieldmap %in% sample_names)
}

load_fieldmap <- function(fieldmap_path) {
  # project_path <- "test_data/example/field_matrices"
  # fieldmap_path <- file.path(project_path, paste0(fieldmap_id, ".txt"))
  fieldmap <- read.csv(fieldmap_path, header = FALSE)
  as.matrix(fieldmap)
}

validate_uploaded_maps <- function(maps, speclib) {
  vapply(field_maps, is_valid_fieldmap, speclib, USE.NAMES = FALSE, FUN.VALUE = logical(1))
}