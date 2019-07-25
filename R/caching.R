#' Caching FinBIF downloads
#'
#' Working with cached data from FinBIF.
#'
#' @section Turning caching off:
#' By default, local filesystem caching of most FinBIF API requests is turned
#' on. Any request made using the same arguments will only request data from
#' FinBIF in the first instance and subsequent requests will use the local
#' cache while it exists. This will increase the speed of repeated requests and
#' save bandwidth and computation for the FinBIF server. Caching can be turned
#' off temporarily by setting the `cache = FALSE` in the requesting function.
#'
#' Setting `options(finbif_use_cache = FALSE)` will turn off caching for the
#' current session.
#'
#' @section Setting the cache directory:
#' By default cached requests are stored in `tempdir()`. This can be changed by
#' setting the file path for the current session with
#' `options(finbif_cache_path = "path/to/cache")`.
#'
#' @section Clearing the cache:
#' The cache files can be removed using `finbif_clear_cache()`.
#'
#' @name caching
NULL
