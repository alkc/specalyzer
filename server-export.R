get_export_table <- function(data, table_type, indices, by_attribute) {
  if(table_type == 'spectra') {
    table <- hsdar::spectra(data)  
    colnames(table) <- hsdar::wavelength(data) %>% as.character()
  } else if(table_type == 'vegindex') {
    if(is.null(indices) | length(indices) < 1) {
      stop("Missing indices to include in exported table")
    }
    table <- specalyzer::vegindex(data, indices)
  }
  if(!is.null(by_attribute)) {
    attrib <- get_selected_attribs(data, by_attribute) %>% unlist
    table <- aggregate(x = table, by = list(attrib), FUN = "mean")
    colnames(table)[1] <- by_attribute
    row.names(table) <- NULL
  } else {
    file_id = idSpeclib(data)
    table <- data.frame(file_id, table, stringsAsFactors = FALSE, check.names = FALSE )
  }
  table
}