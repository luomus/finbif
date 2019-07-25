#' Clear cache
#'
#' Remove cached API requests from FinBIF.
#'
#' @return 0 for success, 1 for failure, invisibly.
#' @examples \dontrun{
#'
#' finbif_clear_cache()
#' }
#' @export
finbif_clear_cache <- function() {
  fcp <- getOption("finbif_cache_path")
  fcp <- if (is.null(fcp)) tempdir()
  unlink(file.path(fcp, "finbif_cache_file_*"))
}
