#' Clear cache
#'
#' Remove cached FinBIF API requests.
#'
#' @aliases fb_clear_cache
#'
#' @examples \dontrun{
#'
#' finbif_clear_cache()
#'
#' }
#' @export
finbif_clear_cache <- function() {
  fcp <- getOption("finbif_cache_path")

  if (is.null(fcp)) {
    cache_list <- ls(all.names = TRUE, envir = cache_location)
    rm(list = cache_list, envir = cache_location)
  } else if (is.character(fcp)) {
    cache_files <- file.path(fcp, "finbif_cache_file_*")
    cache_files_deleted <- unlink(cache_files)
    cache_files_deleted <- identical(cache_files_deleted, 0L)
    stopifnot("Cache file deletion failed" = cache_files_deleted)
  } else {
    stopifnot("Package {DBI} needed to use a DB cache" = has_pkgs("DBI"))

    if (DBI::dbExistsTable(fcp, "finbif_cache")) {
      DBI::dbRemoveTable(fcp, "finbif_cache")
    }

  }

  invisible(NULL)
}
