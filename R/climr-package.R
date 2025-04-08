#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    do.call(
      sprintf,
      c(
        list(fmt = "Welcome to climr %s.%s.%s!"),
        utils::packageVersion("climr") |> unlist() |> head(3) |> as.list()
      )
    )
  )
}

# On load, instantiate either as new or from cache
#' @importFrom utils packageVersion
#' @noRd
.onLoad <- function(libname, pkgname) {

  init_globals()
  options("climr.recycle.warn" = TRUE)

}