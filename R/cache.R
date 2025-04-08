# Cache utilities

#' Return package local cache path
#'
#' @details By default, it uses [tools::R_user_dir()]. The cache location can be
#' set using the `climr.cache.path` option with `options("climr.cache.path" = "your_path")` or
#' use environment variable `CLIMR_CACHE_PATH`.
#'
#' @return character. The full path of the package local cache.
#'
#' @export
#' @rdname Caching
#' @importFrom tools R_user_dir
cache_path <- function() {
  getOption("climr.cache.path", default = Sys.getenv("CLIMR_CACHE_PATH", unset = tools::R_user_dir("climr", "cache")))
}

#' Check if package local cache exists
#'
#' @return logical.
#' @noRd
cache_exists <- function() {
  file.exists(cache_path())
}

#' Ask if user want to use local cache, otherwise use a temporary directory.
#'
#' @noRd
#' @param ask logical. Ask before deleting files. Default to `interactive()`.
#'
#' @return logical.
#'
#' @importFrom utils askYesNo
cache_ask <- function(ask = interactive()) {
  # Only ask if cache does not already exists
  if (!cache_exists() && isTRUE(ask)) {
    response <- utils::askYesNo(
      "Is it ok to cache climr raster files locally?",
      prompts = c("Yes", "No", "Cancel")
    )
    if (is.na(response)) {
      stop("Cancelled by user.", call. = FALSE)
    } else {
      # To avoid asking again in the same session
      options("climr.session.cache.ask.response" = response)
      return(response)
    }
  } else {
    return(TRUE)
  }
}


#' Clear the package's local cache path
#' Attempts to delete all folder/files in `cache_path()`.
#' @details
#'   It may fail if R has no permission to delete files/folders
#'   in the `cache_path()` directory
#' @seealso [cache_path()]
#'
#' @return TRUE or FALSE depending on whether cache was cleared successfully
#'   or not.
#' @rdname Caching
#' @export
cache_clear <- function() {
  unlink(cache_path(), recursive = TRUE, force = TRUE)
}

# Retrieve object from cache
#' @noRd
cache_get <- function(tbl, cache = TRUE) {
  
  dbCon <- data_connect()

  # Early exit if no cache and no database connection
  if (!cache && is.null(dbCon)) {
    stop("Could not connect to database.")
  }
  
  if (cache) {
    # Define cache file path once
    cache_file <- file.path(cache_path(), sprintf("%s.rds", tbl))
    
    # Check cache existence and database connection
    if (!file.exists(cache_file)) {
      if (is.null(dbCon)) stop("No local cache found and could not connect to database.")
      return(fetch_and_cache(tbl, cache_file))
    }
    
    # Read cached data
    res <- .globals[["cache"]][[cache_file]]
    if (is.null(res)) {
      res <- readRDS(cache_file)
      .globals[["cache"]][[cache_file]] <- res
    }
    
    # Update cache if database is newer
    if (!is.null(dbCon)) {
      lastmod <- fetch_last_modified(tbl)
      if (lastmod > attr(res, "last_modified")) {
        return(fetch_and_cache(tbl, cache_file))
      }
    }
    return(res)
  }
  
  # Fetch directly from database if cache is disabled
  "SELECT * FROM %s" |> sprintf(tbl) |> db_safe_query()
}

# Locally cache geotiff
#' @noRd
cache_tif <- function(rastertbl, layers, VAR = NULL, hull = NULL, cache = TRUE) {
  
  rastertbl <- "normal_composite"
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
  xyz <- data.frame(
    lon = runif(500000, -127, -93),
    lat = runif(500000, 52, 57)
  )
  
  # Not cached
  last_mod <- fetch_last_modified(rastertbl)
  
  info <- db_safe_query("
    select
      rid,
      st_xmax(st_envelope(rast)) as xmx,
      st_xmin(st_envelope(rast)) as xmn,
      st_ymax(st_envelope(rast)) as ymx,
      st_ymin(st_envelope(rast)) as ymn,
      st_width(rast) as cols,
      st_height(rast) as rows,
      st_numbands(rast) as nbands,
      ST_AsText(ST_ConvexHull(rast)) as hull,
      false as cached
    from \"%s\"
  " |> sprintf(rastertbl)) |> data.table::setDT()
  
  a <- terra::relate(terra::vect(info$hull), terra::vect(xyz, crs = "EPSG:4326"), "intersects", pairs = TRUE, na.rm = TRUE)
  
  rid <- info[a[,1] |> unique() |> sort(), rid]
  
  
}

# Helper function to fetch data and cache it
#' @noRd
fetch_and_cache <- function(tbl, cache_file) {
  res <- "SELECT * FROM %s" |> sprintf(tbl) |> db_safe_query()
  lastmod <- fetch_last_modified(tbl)
  attr(res, "last_modified") <- lastmod
  cache_file |> dirname() |> dir.create(recursive = TRUE, showWarnings = FALSE)
  saveRDS(res, cache_file)
  .globals[["cache"]][[cache_file]] <- res
  res
}

# Helper function to get last modified timestamp
#' @noRd
fetch_last_modified <- function(tbl) {
  res <- "SELECT last_modified FROM table_modification_log WHERE table_name = '%s'" |>
    sprintf(tbl) |>
    db_safe_query()
  res$last_modified
}
