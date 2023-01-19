#' Clear cache
#'
#' Remove cached API requests from FinBIF.
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

  fcp_is_null <- is.null(fcp)

  if (fcp_is_null) {

    cache_list <- ls(all.names = TRUE, envir = cache_location)

    rm(list = cache_list, envir = cache_location)

  } else {

    cache_files <- file.path(fcp, "finbif_cache_file_*")

    cache_files_deleted <- unlink(cache_files)

    cache_files_deleted <- identical(cache_files_deleted, 0L)

    stopifnot(cache_files_deleted)

    invisible(NULL)

  }

}
