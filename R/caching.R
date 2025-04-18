#' Caching FinBIF downloads
#'
#' Working with cached data from FinBIF.
#'
#' @section Turning caching off:
#' By default, local caching of most FinBIF API requests is turned on. Any
#' request made using the same arguments will only request data from FinBIF in
#' the first instance and subsequent requests will use the local cache while it
#' exists. This will increase the speed of repeated requests and save bandwidth
#' and computation for the FinBIF server. Caching can be turned off temporarily
#' by setting `cache = c(FALSE, FALSE)` in the requesting function.
#'
#' Setting
#' `options(finbif_use_cache = FALSE, finbif_use_cache_metadata = FALSE)` will
#' turn off caching for the current session.
#'
#' @section Using filesystem caching:
#' By default cached requests are stored in memory. This can be changed by
#' setting the file path for the current session with
#' `options(finbif_cache_path = "path/to/cache")`.
#'
#' @section Using database caching:
#' Caching can also be done using a database. Using a database for caching
#' requires the `DBI` package and a database backend package such as `RSQLite`
#' to be installed. To use the database for caching simply pass the connection
#' objected created with `DBI::dbConnect` to the `finbif_cache_path` option
#' (e.g.,
#' `db <- DBI::dbConnect(RSQLite::SQLite(), "my-db.sqlite"); `
#' `options(finbif_cache_path = db)`
#' ).
#'
#' @section Timeouts:
#' A cache timeout can be set by using an integer (number of hours until cache
#' is considered invalid and is cleared) instead of a logical value for the
#' `finbif_use_cache` and `finbif_use_cache_metadata` options or the `cache`
#' function arguments.
#'
#' @section Clearing the cache:
#' The cache can be reset `finbif_clear_cache()`.
#'
#' @section Updating the cache:
#' The cache can be updated using `finbif_update_cache()`.
#'
#' @name caching
NULL

#' @noRd
cache_location <- new.env()

#' @noRd
get_cache <- function(hash) {
  data <- NULL

  if (exists(hash, envir = cache_location)) {
    obj <- get(hash, envir = cache_location)
    valid <- TRUE

    if (!isTRUE(obj[["swagger"]])) {
      valid <- cache_is_valid(obj[["timeout"]], obj[["created"]])
    }

    if (valid) {
      data <- obj[["data"]]
    } else {
      remove(list = hash, envir = cache_location)
    }
  }

  data
}

#' @noRd
set_cache <- function(obj) {
  obj[["created"]] <- Sys.time()
  assign(obj[["hash"]], obj, envir = cache_location)
}
