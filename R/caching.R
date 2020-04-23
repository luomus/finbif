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
#' by setting the `cache = FALSE` in the requesting function.
#'
#' Setting `options(finbif_use_cache = FALSE)` will turn off caching for the
#' current session.
#'
#' @section Using filesystem caching:
#' By default cached requests are stored in memory. This can be changed by
#' setting the file path for the current session with
#' `options(finbif_cache_path = "path/to/cache")`.
#'
#' @section Clearing the cache:
#' The cache files can deleted `finbif_clear_cache()`.
#'
#' @name caching
NULL

#' @noRd

cache_location <- new.env()

get_cache <- function(hash)
  if (exists(hash, envir = cache_location)) get(hash, envir = cache_location)

set_cache <- function(ans, hash) assign(hash, ans, envir = cache_location)
