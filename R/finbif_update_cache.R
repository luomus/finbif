#' Update cache
#'
#' Update all cached FinBIF API requests.
#'
#' @aliases fb_update_cache
#'
#' @examples \dontrun{
#'
#' finbif_update_cache()
#'
#' }
#' @export
finbif_update_cache <- function() {
  fcp <- getOption("finbif_cache_path")

  if (is.null(fcp)) {
    for (cached_object in as.list(cache_location)) {
      is_swagger <- isTRUE(cached_object[["swagger"]])
      can_be_updated <- !is.character(cached_object[["data"]]) && !is_swagger

      if (can_be_updated) {
        api_get(cached_object[["data"]])
      }

    }
  } else if (is.character(fcp)) {
    for (cached_object in list.files(fcp, pattern = "finbif_cache_file_")) {
      cached_object <- file.path(fcp, cached_object)
      cached_object <- readRDS(cached_object)
      api_get(cached_object)
    }
  } else {
    stopifnot("Package {DBI} needed to use a DB cache" = has_pkgs("DBI"))

    if (DBI::dbExistsTable(fcp, "finbif_cache")) {
      cached_objects <- DBI::dbGetQuery(fcp, "SELECT hash FROM finbif_cache")

      for (i in cached_objects[["hash"]]) {
        db_query_i <- sprintf("SELECT * FROM finbif_cache WHERE hash = '%s'", i)
        db_cache <- DBI::dbGetQuery(fcp, db_query_i)
        created <- as.POSIXct(db_cache[["created"]], origin = "1970-01-01")
        last_cache_ind <- which.max(created)[[1L]]
        cached_object <- unserialize(db_cache[last_cache_ind, "blob"][[1L]])
        api_get(cached_object)
      }

    }
  }

  invisible(NULL)
}
