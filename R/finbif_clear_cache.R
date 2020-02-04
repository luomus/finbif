#' Clear cache
#'
#' Remove cached API requests from FinBIF.
#'
#' @examples \dontrun{
#'
#' finbif_clear_cache()
#' }
#' @export
finbif_clear_cache <- function() {
  fcp <- getOption("finbif_cache_path")
  if (is.null(fcp)) {
    rm(
      list = ls(all.names = TRUE, envir = cache_location),
      envir = cache_location
    )
  } else {
    stopifnot(identical(unlink(file.path(fcp, "finbif_cache_file_*")), 0L))
    invisible(NULL)
  }
}
