#' Update cache
#'
#' Update all cached API requests from FinBIF.
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

  in_memory <- is.null(fcp)

  in_file_system <- is.character(fcp)

  if (in_memory) {

    cached_objects <- as.list(cache_location)

    for (i in cached_objects) {

      cached_object <- i[["data"]]

      is_swagger <- i[["swagger"]]

      is_swagger <- isTRUE(is_swagger)

      is_write_file <- is.character(cached_object)

      can_be_updated <- !is_write_file && !is_swagger

      if (can_be_updated) {

        api_get(cached_object)

      }

    }

  } else if (in_file_system) {

    cached_objects <- list.files(fcp, pattern = "finbif_cache_file_")

    for (i in cached_objects) {

      cached_object <- file.path(fcp, i)

      cached_object <- readRDS(cached_object)

      api_get(cached_object)

    }

  } else {

    has_dbi <- has_pkgs("DBI", "blob")

    stopifnot("Packages {DBI} & {blob} needed to use a DB cache" = has_dbi)

    has_table <- DBI::dbExistsTable(fcp, "finbif_cache")

    if (has_table) {

      db_query <- "SELECT hash FROM finbif_cache"

      cached_objects <- DBI::dbGetQuery(fcp, db_query)

      cached_objects <- cached_objects[["hash"]]

      db_query <- "SELECT * FROM finbif_cache WHERE hash = '%s'"

      for (i in cached_objects) {

        db_query_i <- sprintf(db_query, i)

        db_cache <- DBI::dbGetQuery(fcp, db_query_i)

        created <- db_cache[["created"]]

        created <- as.POSIXct(created, origin = "1970-01-01")

        last_cache_ind <- which.max(created)

        last_cache_ind <- last_cache_ind[[1L]]

        cached_object <- db_cache[last_cache_ind, "blob"]

        cached_object <- cached_object[[1L]]

        cached_object <- unserialize(cached_object)

        api_get(cached_object)

      }

    }

  }

  invisible(NULL)

}
