#' Retrieve global climate model anomalies for `downscale_core`.
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with [`downscale_core()`].
#'
#' @description
#' `input_gcms` retrieves anomalies of 20-year periods for selected GCMs, SSPs,
#'  periods and runs.
#'
#' @template gcms
#' @template ssps
#' @template period
#' @template max_run
#' @template run_nm
#' @template cache
#' @template raster
#' @return This function returns a list with one slot for each requested GCM. Rasters inside the
#' list contain anomalies for all requested SSPs, runs, and periods. In general this function should
#' only be used in combination with [`downscale_core()`]. If `raster` is `FALSE`, an input list
#' defining data source, `tbl` and `layers`.
#' @seealso [downscale_core()]
#' @import data.table
#'
#' @examples
#' library(terra)
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)
#'
#' ## get bounding box based on input points
#' thebb <- get_bb(xyz)
#'
#' ## get database connection
#' dbCon <- data_connect()
#'
#' gcms <- input_gcms(dbCon, thebb, list_gcms()[1], list_ssps()[1])
#'
#' ## show ensemble means only
#' lyrs <- grep("ensemble", names(gcms$`ACCESS-ESM1-5`))
#'
#' plot(gcms$`ACCESS-ESM1-5`[[lyrs]])
#' @rdname gcms-input-data
#' @export
input_gcms <- function(
  gcms = list_gcms(),
  ssps = list_ssps(),
  period = list_gcm_periods(),
  max_run = 0L,
  run_nm = NULL,
  cache = TRUE,
  raster = TRUE
){
  
  gcms <- match.arg(gcms, list_gcms(), several.ok = TRUE)
  ssps <- match.arg(ssps, list_ssps(), several.ok = TRUE)
  period <- match.arg(period, list_gcm_periods(), several.ok = TRUE)
  
  if (!is(max_run, "numeric")) {
    stop("please pass a numeric value to 'max_runs'")
  }
  
  esm_layers_period <- cache_get("esm_layers_period", cache) |> data.table::setDT()
  runs <- unique(esm_layers_period[,list(mod, scenario, run)])
  p <- period
  
  res <- lapply(gcms, function(gcm_nm) {
    gcmcode <- dbnames[GCM == gcm_nm, dbname]
    runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssps, run]))
    if (is.null(run_nm)) {
      sel_runs <- runs[1:(max_run + 1L)]
    } else {
      if (!run_nm %in% runs) {
        stop("Run ", run_nm, "doesn't exist for this GCM.")
      }
      sel_runs <- run_nm
    }
    list(
      tbl = gcmcode,
      layers = esm_layers_period[
        mod %in% gcm_nm & scenario %in% ssps & period %in% p & run %in% sel_runs,
        list(
          var_nm = paste(mod, var, month, scenario, run, period, sep = "_"),
          laynum
        )
      ]
    )
  })
  
  if (isTRUE(raster)) {
    res <- cache_tif(res, cache)
  }
  
  attr(res, "builder") <- "climr"
  return(res)
  
}


#' @description
#' `input_gcm_hist` creates GCM time series inputs for the historical scenario (1850-2014),
#' given chosen GCMs, years and runs.
#'
#' @template gcms
#' @param years numeric. Vector of desired years. Default is `1901:2014`.
#'   See [`list_gcm_hist_years()`] for available years.
#' @template max_run
#' @template run_nm
#' @template cache
#' @template raster
#'
#' @seealso [list_gcm_periods()], [`list_gcm_periods()`]
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can be used with
#' [`downscale_core()`]. If `raster` is `FALSE`, an input list defining data source, `tbl`
#' and `layers`.
#'
#' @details This function returns a list with one slot for each requested GCM. Rasters inside the
#' list contain anomalies for all runs and years. In general this function should only be used in
#' combination with [`downscale_core()`].
#' @import data.table
#' @rdname gcms-input-data
#' @export
input_gcm_hist <- function(
  gcms = list_gcms(),
  years = 1901:2014,
  max_run = 0L,
  run_nm = NULL,
  cache = TRUE,
  raster = TRUE
){
  
  gcms <- match.arg(gcms, list_gcms(), several.ok = TRUE)
  
  esm_layers_hist <- cache_get("esm_layers_hist", cache) |> data.table::setDT()
  runs <- unique(esm_layers_hist[,list(mod, run)])
  
  res <- lapply(gcms, function(gcm_nm) {
    if (!gcm_nm %in% dbnames_hist$GCM) return(NULL)
    gcmcode <- dbnames_hist[GCM == gcm_nm, dbname]
    runs <- sort(unique(runs[mod == gcm_nm, run]))
    if (is.null(run_nm)) {
      sel_runs <- runs[1:(max_run + 1L)]
    } else {
      if (!run_nm %in% runs) {
        stop("Run ", run_nm, "doesn't exist for this GCM.")
      }
      sel_runs <- run_nm
    }
    
    list(
      tbl = gcmcode,
      layers = esm_layers_hist[
        mod %in% gcm_nm & year %in% years & run %in% sel_runs,
        list(
          var_nm = paste(mod, var, month, run, year, sep = "_"),
          laynum
        )
      ]
    )
  })
  
  res <- res[!sapply(res, is.null)] ## remove NULL
  
  if (isTRUE(raster)) {
    res <- cache_tif(res, cache)
  }
  
  attr(res, "builder") <- "climr"
  return(res)
  
}

#' @description
#' `input_gcm_ssp` creates future GCM time series inputs, given chosen GCMs, SSPs,
#'  years and runs.
#'
#' @template gcms
#' @template ssps
#' @param years Numeric or character vector in `2020:2100`. Defaults to `2020:2030`.
#'   See [`list_gcm_ssp_years()`] for available years.
#' @template max_run
#' @template run_nm
#' @template cache
#' @template raster
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can be used with
#' [`downscale_core()`]. If `raster` is `FALSE`, an input list defining data source, `tbl`
#' and `layers`.
#'
#' @details This function returns a list with one slot for each requested GCM. Rasters inside the
#' list contain anomalies for all SSPs, runs and years. In general this function should only be
#' used in combination with [`downscale_core()`]. Note that if you request multiple runs, multiple
#' SSPs, and a lot of years, it will take a while to download the data (there's lot of it).
#'
#' @import data.table
#'
#' @rdname gcms-input-data
#' @export
input_gcm_ssp <- function(
  gcms = list_gcms(),
  ssps = list_ssps(),
  years = 2020:2030,
  max_run = 0L,
  run_nm = NULL,
  cache = TRUE,
  raster = TRUE
){
  
  if (nrow(dbnames_ts) < 1) stop("That isn't a valid GCM")
  
  esm_layers_ts <- cache_get("esm_layers_ts", cache) |> data.table::setDT()
  runs <- unique(esm_layers_ts[,list(mod, scenario, run)])
  
  res <- lapply(gcms, function(gcm_nm) {
    if (!gcm_nm %in% dbnames_ts[["GCM"]]) return()
    gcmcode <- dbnames_ts[GCM == gcm_nm, dbname]
    runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssps, run]))
    if (length(runs) < 1) {
      warning("That GCM isn't in our database yet.")
      return()
    }
    
    if (is.null(run_nm)) {
      sel_runs <- runs[1:(max_run + 1L)]
    } else {
      if (!run_nm %in% runs) {
        stop("Run ", run_nm, "doesn't exist for this GCM.")
      }
      sel_runs <- run_nm
    }
    
    list(
      tbl = gcmcode,
      layers = esm_layers_ts[
        mod %in% gcm_nm & scenario %in% ssps & period %in% years & run %in% sel_runs,
        list(
          var_nm = fullnm,
          laynum
        )
      ],
      VAR = c("PPT", "Tmin", "Tmax")
    )
  })
  
  res <- res[!sapply(res, is.null)] ##remove NULL
  
  if (isTRUE(raster)) {
    res <- cache_tif(res, cache)
  }
  
  attr(res, "builder") <- "climr"
  return(res)
  
}
