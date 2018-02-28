# This function is a wrapper around hsdar's vegindex() function in order
# to expand the list of built-in indices
#' @export
vegindex <- function(data, index, ...) {

  # Get the file with PlantSpec veg indices and read the expressions
  path <- file.path("extdata", "vegetation_indices.txt") %>%
    system.file(package="specalyzer")

  veg_indices <- read.table(path, header = FALSE, sep ="\t",
                            comment.char = '#',
                            stringsAsFactors = FALSE)

  colnames(veg_indices) <- c("name", "expression")
  hsdar_veg_indices <- hsdar::vegindex()

  # If no args provided, return list of available indices
  if(length(names(match.call())) == 0) {
    available_indices <- c(hsdar_veg_indices, veg_indices[,1]) %>%
      unique %>% sort
    return(available_indices)
  }

  # Sub-func for determining if vegindex is in hsdar or PlantSpec
  which_pkg <- function(index) {
    if(index %in% hsdar_veg_indices) {
      val <- index
    } else if(index %in% veg_indices[,1]) {
      i <- which(veg_indices[,1] == index)
      val <- veg_indices[i,2]
    } else {
      val <- index
    }
    return(val)
  }

  # Build index arg to hsdar::vegindex()
  index_call <- unlist(lapply(index, which_pkg))

  result <- hsdar::vegindex(data, index_call, ...)

  # Update the table header
  if(typeof(result) != "list") {
    result <- as.data.frame(result)
    rownames(result) <- idSpeclib(data)
    colnames(result) <- index
  } else {
    names(result) <- index
  }
  return(result)
}

#' @export
get_available_indices <- function() {
  vegindex()
}
