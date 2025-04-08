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
  getOption("climr.cache.path", default = Sys.getenv("CLIMR_CACHE_PATH", unset = utils::R_user_dir("climr", "cache")))
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
    res <- readRDS(cache_file)
    
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

# Helper function to fetch data and cache it
#' @noRd
fetch_and_cache <- function(tbl, cache_file) {
  res <- "SELECT * FROM %s" |> sprintf(tbl) |> db_safe_query()
  lastmod <- fetch_last_modified(tbl)
  attr(res, "last_modified") <- lastmod
  cache_file |> dirname() |> dir.create(recursive = TRUE, showWarnings = FALSE)
  saveRDS(res, cache_file)
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
