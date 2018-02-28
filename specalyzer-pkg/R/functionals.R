#' @export
mask_ <- function(data, value) {
  hsdar::mask(data) <- value
  data
}
