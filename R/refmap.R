#' Retrieve reference period climate maps
#' @description
#' This function downloads (or retrieves from cache) monthly Tmin, Tmax, and PPT climatologies (maps of long-term average climate)
#' from a specified data source for the specified bounding box.
#' It is intended for use with [`downscale_core()`], but can also be used as stand-alone raster data.
#'
#' @template reference
#' @template cache
#'
#' @return An input list, with two nodes, `tbl` and `layers`, suitable for `extract_rast_tbl`.
#'
#' @details
#' The first 36 layers of the output raster correspond with the actual climate variables. The raster also contains
#' lapse rates for each variable, and a corresponding digital elevation model.
#'
#'
#' @seealso [downscale()]
#'
#' @import data.table
#' @rdname input_refmap
#' @export
input_refmap <- function(reference = "refmap_climr", cache = TRUE) {
  
  #Remove NSE CRAN check warnings
  if (FALSE){ var_nm <- laynum <- NULL}
  
  rmap_nm <- reference
  if (!grepl("normal", reference)) {
    rmap_nm <- switch(reference,
                      refmap_prism = "normal_bc",
                      refmap_climr = "normal_composite",
                      refmap_climatena = "normal_na",
                      auto = "normal_composite"
    )
  }
  
  layers <- data.table::data.table(var_nm = c(
    "PPT_01", "PPT_02", "PPT_03", "PPT_04", "PPT_05", "PPT_06", "PPT_07",
    "PPT_08", "PPT_09", "PPT_10", "PPT_11", "PPT_12", "Tmax_01", "Tmax_02",
    "Tmax_03", "Tmax_04", "Tmax_05", "Tmax_06", "Tmax_07", "Tmax_08", "Tmax_09",
    "Tmax_10", "Tmax_11", "Tmax_12", "Tmin_01", "Tmin_02", "Tmin_03", "Tmin_04",
    "Tmin_05", "Tmin_06", "Tmin_07", "Tmin_08", "Tmin_09", "Tmin_10", "Tmin_11",
    "Tmin_12", "lr_PPT_01", "lr_PPT_02", "lr_PPT_03", "lr_PPT_04", "lr_PPT_05",
    "lr_PPT_06", "lr_PPT_07", "lr_PPT_08", "lr_PPT_09", "lr_PPT_10", "lr_PPT_11",
    "lr_PPT_12", "lr_Tmax_01", "lr_Tmax_02", "lr_Tmax_03", "lr_Tmax_04",
    "lr_Tmax_05", "lr_Tmax_06", "lr_Tmax_07", "lr_Tmax_08", "lr_Tmax_09",
    "lr_Tmax_10", "lr_Tmax_11", "lr_Tmax_12", "lr_Tmin_01", "lr_Tmin_02",
    "lr_Tmin_03", "lr_Tmin_04", "lr_Tmin_05", "lr_Tmin_06", "lr_Tmin_07",
    "lr_Tmin_08", "lr_Tmin_09", "lr_Tmin_10", "lr_Tmin_11", "lr_Tmin_12",
    "dem2_WNA"
  ))
  layers[, laynum := seq_along(var_nm)]
  res <- list(
    tbl = rmap_nm,
    layers = layers
  )
  attr(res, "builder") <- "climr"
  return(res)
  
}
