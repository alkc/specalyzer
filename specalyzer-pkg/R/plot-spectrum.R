#' @include util.R
#' @export
plot_spectrum <- function(data, aggregate_by = NULL,
                          type = 'mean', plot_individual_spectra = FALSE, ...) {

  if(!plot_individual_spectra) {
    p <- plot_aggregate_spectrum(data, aggregate_by, type, ...)
  } else {
    p <- plot_individual_spectra(data, ...)
  }
  process_spectral_plot(p)
}

plot_aggregate_spectrum <- function(data, aggregate_by, type, ...) {

  fun <- get_aggregate_function(type)
  plot_data <- data %>%
    build_spectral_plot_data(byattributes = aggregate_by, fun = fun)

  if(!is.null(aggregate_by)) {
    split_by <- as.formula(paste("~", aggregate_by, sep = ""))
  } else {
    split_by <- NULL
  }

  plotly::plot_ly(plot_data,
                  x = ~Wavelength,
                  y= ~Reflectance,
                  split = split_by,
                  colors = split_by,
                  type = 'scatter',
                  mode = 'lines',
                  connectgaps = FALSE, ...)
}

process_spectral_plot <- function(p) {

  margin <- list(b = 70)

  ticks <-  list(
    family = "monospace",
    size = 10,
    color = "#7f7f7f")

  xaxis = list(tickangle = 70,
               showgrid = TRUE,
               tickmode = "auto",
               ticks ="outside")

  plotly::layout(p, xaxis = xaxis, margin = margin)

}

plot_individual_spectra <- function(data, ...) {

  spectral_data <- get_masked_spectra(data) %>%
    get_long_spectral_data(grp = idSpeclib(data))

  colnames(spectral_data) <- c("Id", "Wavelength", "Reflectance")
  spectral_data <- spectral_data %>% arrange(Wavelength, Id)

  plotly::plot_ly(
    spectral_data,
    x = ~Wavelength,
    y= ~Reflectance, type ='scatter',
    split = ~Id,
    mode ='lines',
    connectgaps = FALSE)

}

get_long_spectral_data <- function(data, grp) {

  new_names <- c("grp", colnames(data))
  data <- data.frame(grp, data)
  colnames(data) <- new_names

  long_data <- reshape2::melt(data, id.vars = 1, value.name = "Reflectance",
                              variable.name = "Wavelength")

  long_data[['Wavelength']] <- long_data[['Wavelength']] %>%
    as.character() %>% as.numeric()

  long_data[['grp']] <- try_coercing_grp_to_int(long_data[['grp']])
  long_data

}

try_coercing_grp_to_int <- function(grp_col) {
  result <- tryCatch({
    grp_col %>% as.character %>% as.numeric()
  }, warning = function(cond) {
    return(grp_col)

  }, error = function(cond) {
    return(grp_col)
  })

  result

}

build_spectral_plot_data <- function(x, byattributes, fun) {

  mask_bkp <- attr(x, "dropped")
  x <- apply(x, FUN = fun, byattributes = byattributes)
  attr(x, "dropped") <- mask_bkp
  spectral_data <- get_masked_spectra(x)

  if(!is.null(byattributes)) {
    grp <- attribute(x)[,1]
    plot_data <- get_long_spectral_data(spectral_data, grp = grp)
    colnames(plot_data)[1] <- byattributes

  } else {
    plot_data <- data.frame(Wavelength = colnames(spectral_data) %>% as.numeric(),
                            Reflectance = spectral_data[1,] %>% unlist())


  }

  plot_data %>% arrange(Wavelength)

}

get_aggregate_function <- function(type) {

  allowed_functions <- c('mean', 'median', 'var', 'sd')

  if(type[1] %in% allowed_functions) {
    type
  } else {
    e <- paste0("Unrecognized plot type. Available functions are:",
                paste(allowed_functions, collapse = ", "), ".")
    stop(e)
    }
}
