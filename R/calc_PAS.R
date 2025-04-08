#' Calculate Precipitation As Snow (PAS)
#'
#' @template m
#' @template tmin
#' @template PPT
#'
#' @return numeric. Precipitation As Snow
#'
#' @examples
#' \dontrun{
#' climr::calc_PAS(4, 2, 600)
#' }
#' @export
#' @rdname climatevar
calc_PAS <- function(m, tave, PPT) {
  param[["PAS"]][m, PPT * a / (1 + exp(-(tave - T0) / b))]
}
