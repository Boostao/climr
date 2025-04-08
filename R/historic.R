#' Retrieve observational anomalies.
#'
#' @description
#' `input_obs` produces anomalies of the average observed climate for a given **period**,
#' relative to the 1961-1990 reference period. The anomalies are calculated from the `"cru.gpcc"` dataset
#' which is the Climatic Research Unit TS dataset (for temperature) and Global Precipitation Climatology Centre dataset
#' (for precipitation).
#'
#' @return An input list, with two nodes, `tbl` and `layers`, suitable for `extract_rast_tbl`.
#'
#' @param period character. Vector of labels of the periods to use.
#'   Can be obtained from [`list_obs_periods()`]. Default to "2001_2020".
#' @template cache
#'
#' @seealso [downscale_core()], [`list_obs_periods()`]
#' @details
#' Generally, this function should only be used in combination with [`downscale_core()`] as the values
#' returned in the rasters are anomalies compared to the 1961-1990 reference period,
#' and are usually not meaningful without the whole downscale process.
#' @import data.table
#' @rdname hist-input-data
#' @export
input_obs <- function(period = list_obs_periods(), cache = TRUE) {
  
  #Remove NSE CRAN check warnings
  if (FALSE) { var_nm <- laynum <- period <- NULL}
  
  dbnames2 <- structure(list(
    PERIOD = c("2001_2020"),
    dbname = c("historic_periods")
  ), class = "data.frame", row.names = c(NA, -1L))
  
  dbcode <- dbnames2$dbname[dbnames2$PERIOD %in% period]
  
  historic_layers <- cache_get("historic_layers", cache) |> data.table::setDT()
  i <- which(historic_layers[["period"]] %in% period)
  layerinfo <- historic_layers[i, list(var_nm, laynum)]
  
  res <- list("obs" = list(
    tbl = dbcode,
    layers = layerinfo
  ))
  attr(res, "builder") <- "climr"
  return(res)
  
}


#' Retrieve observational anomalies for specified years
#' @description
#' `input_obs_ts` produces anomalies of observed climate for a given **time series** of individual years.
#'
#' @param dataset Character. Which observational dataset to use? Options are `"climatena"` for the
#' ClimateNA gridded time series or `"cru.gpcc"` for the combined Climatic Research Unit TS dataset
#' (for temperature) and Global Precipitation Climatology Centre dataset (for precipitation).
#' @template cache
#' @param years numeric. Years to retrieve obs anomalies for. Defaults to `2010:2022`.
#'   See [`list_obs_years()`] for available years.
#' @return An input list, with two nodes, `tbl` and `layers`, suitable for `extract_rast_tbl`.
#' @details
#' The returned raster contains anomalies for each year specified in `years`. In general this
#' function should only be used in conjunction with [`downscale_core()`].
#' @import data.table
#' @rdname hist-input-data
#' @export
input_obs_ts <- function(dataset = c("cru.gpcc", "climatena"), years = 2010:2022, cache = TRUE) {

  #Remove NSE CRAN check warnings
  if (FALSE){ var_nm <- period <- laynum <- NULL}
  
  res <- lapply(dataset, function(d) {
    if (is.na(m <- match(d, dbnames_hist_obs$dataset))) return(NULL)
    dbcode <- dbnames_hist_obs[["dbname"]][m]
    layers <- cache_get(paste(dbcode, "layers", sep = "_"), cache) |> data.table::setDT()
    layerinfo <- layers[period %in% years, list(var_nm = paste(d, var_nm, period, sep = "_"), laynum)]
    list(tbl = dbcode, layers = layerinfo)
  })
  
  res <- res[!sapply(res, is.null)] ## remove NULL
  attr(res, "builder") <- "climr"
  return(res)
  
}
