#' List available runs, global climate models (GCMs), time periods and scenarios (SSPs)
#'
#' @return a character vector.
#'
#' @description
#' `list_gcms` lists available global climate models.
#'
#' @rdname data-option-lists
#' @export
list_gcms <- function() {
  res <- .globals[["cache"]][["list_gcms"]]
  if (is.null(res)) {
    res <- cache_get("esm_layers_period")
    res <- sort(unique(res[["mod"]]))
    .globals[["cache"]][["list_gcms"]] <- res
  }
  return(res)
}

#' @description
#' `list_ssps` lists available greenhouse gas concentration scenarios (SSP-RCPs).
#'
#' @rdname data-option-lists
#' @export
list_ssps <- function() {
  res <- .globals[["cache"]][["list_ssps"]]
  if (is.null(res)) {
    res <- cache_get("esm_layers_period")
    res <- sort(unique(res[["scenario"]]))
    .globals[["cache"]][["list_ssps"]] <- res
  }
  return(res)
}

#' @description
#' `list_gcm_periods` lists available 20-year normal periods for GCM simulations.
#'
#' @rdname data-option-lists
#' @export
list_gcm_periods <- function() {
  res <- .globals[["cache"]][["list_gcm_periods"]]
  if (is.null(res)) {
    res <- cache_get("esm_layers_period")
    res <- sort(unique(res[["period"]]))
    .globals[["cache"]][["list_gcm_periods"]] <- res
  }
  return(res)
}


#' @description
#' `list_runs_ssp` lists available runs for a given GCM/ssp.
#' @param gcm Name of GCM. Must be one of the elements in list_gcms().
#' @param ssp Name of scenario Must be one of the elements in list_ssps().
#' @importFrom data.table fread
#' @importFrom tools R_user_dir
#' 
#' @rdname data-option-lists
#' @export
list_runs_ssp <- function(gcm, ssp) {
  res <- .globals[["cache"]][["list_runs_ssp"]]
  if (is.null(res)) {
    res <- cache_get("esm_layers_period") |> data.table::setDT()
    res <- unique(res[,list(mod, scenario, run)])
    .globals[["cache"]][["list_runs_ssp"]] <- res
  }
  res[mod %in% gcm & scenario %in% ssp, sort(unique(run))]
}

#' @description
#' `list_runs_historic` lists available runs from the historical simulation (1851-2014) for a specified GCM.
#' @param gcm Name of GCM
#' @importFrom data.table fread
#' @importFrom tools R_user_dir
#' 
#' @rdname data-option-lists
#' @export
list_runs_historic <- function(gcm){
  res <- .globals[["cache"]][["list_runs_historic"]]
  if (is.null(res)) {
    res <- cache_get("esm_layers_hist") |> data.table::setDT()
    res <- unique(res[,list(mod, run)])
    .globals[["cache"]][["list_runs_historic"]] <- res
  }
  res[mod %in% gcm, sort(unique(run))]  
}

#' @description
#' `list_refmaps` lists available reference maps of gridded climate normals
#'
#' @details
#' Currently available reference maps of gridded climate normals (`list_refmaps()`) are:
#'   * "refmap_climatena" for Climate NA derived normals
#'   * "refmap_prism" for British Columbia PRISM climatologies derived normals
#'   * "refmap_climr" for a composite of BC PRISM, adjusted US PRISM and
#'     DAYMET (Alberta and Saskatchewan), covering western Canada and western
#'     US.
#'
#' @rdname data-option-lists
#' @export
list_refmaps <- function() {
  c("refmap_climatena", "refmap_prism", "refmap_climr")
}


#' @description
#' `list_obs_periods` lists available normal periods for observational climate data
#'
#' @rdname data-option-lists
#' @export
list_obs_periods <- function() {
  res <- .globals[["cache"]][["list_obs_periods"]]
  if (is.null(res)) {
    res <- cache_get("historic_layers")
    res <- sort(unique(res[["period"]]))
    .globals[["cache"]][["list_obs_periods"]] <- res
  }
  return(res)
}

#' @description
#' `list_vars` lists available climate variables
#'
#' @param set character. One of All, Monthly, Seasonal, Annual, or any combination thereof. Defaults to "All".
#' @param only_extra logical. Should Tmin, Tmax and PPT be excluded? Defaults to FALSE.
#'
#' @rdname data-option-lists
#' @export
list_vars <- function(set = c("All", "Monthly", "Seasonal", "Annual"), only_extra = FALSE) {
  set <- match.arg(set, several.ok = TRUE)
  if ("All" %in% set) {
    res <- climr::variables[["Code"]]
  } else {
    res <- climr::variables[["Code"]][climr::variables[["Category"]] %in% set]
  }
  if (isTRUE(only_extra)) {
    res <- res[!grepl("(^PPT|^Tmax|^Tmin)", res)]
  }
  return(sort(unique(res)))
}


#' @description
#' `list_obs_years` lists available years for time series of observational climate data
#'
#' @rdname data-option-lists
#' @export
list_obs_years <- function() {
  res <- .globals[["cache"]][["list_obs_years"]]
  if (is.null(res)) {
    res <- lapply(dbnames_hist_obs[["dbname"]], \(x) {
      res <- "%s_layers" |> sprintf(x) |> cache_get()
      sort(unique(res[["period"]]))
    }) |> unlist() |> unique() |> sort()
    .globals[["cache"]][["list_obs_years"]] <- res
  }
  return(res)
}

#' @description
#' `list_gcm_ssp_years` lists available years for time series of global climate model future simulations 
#'
#' @rdname data-option-lists
#' @export
list_gcm_ssp_years <- function() {
  res <- .globals[["cache"]][["list_gcm_ssp_years"]]
  if (is.null(res)) {
    res <- cache_get("esm_layers_ts")
    res <- sort(unique(res[["period"]]))
    .globals[["cache"]][["list_gcm_ssp_years"]] <- res
  }
  return(res)
}

#' @description
#' `list_gcm_hist_years` lists available years for time series of global climate model historical simulations 
#'
#' @rdname data-option-lists
#' @export
list_gcm_hist_years <- function() {
  res <- .globals[["cache"]][["list_gcm_hist_years"]]
  if (is.null(res)) {
    res <- cache_get("esm_layers_hist")
    res <- sort(unique(res[["year"]]))
    .globals[["cache"]][["list_gcm_hist_years"]] <- res
  }
  return(res)
}
