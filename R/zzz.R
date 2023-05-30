#' @noRd

.onLoad <- function(libname, pkgname) {

  op_finbif <- list(
    finbif_api_url = "https://api.laji.fi",
    finbif_dl_url = "https://dw.laji.fi/download",
    finbif_api_version = "v0",
    finbif_warehouse_query = "warehouse/query/",
    finbif_allow_query = TRUE,
    finbif_hide_progress = FALSE,
    finbif_use_async = TRUE,
    finbif_rate_limit = 1,
    finbif_max_queries = 2000L,
    finbif_max_page_size = 1000L,
    finbif_retry_times = 3L,
    finbif_retry_pause_base = 1L,
    finbif_retry_pause_cap = 60L,
    finbif_retry_pause_min = 1L,
    finbif_use_cache = TRUE,
    finbif_use_cache_metadata = FALSE,
    finbif_timeout_offset = 0,
    finbif_tz = Sys.timezone(),
    finbif_locale = get_locale()
  )

  op <- options()

  toset <- !names(op_finbif) %in% names(op)

  if (any(toset)) {

    options(op_finbif[toset])

  }

  invisible(NULL)

}

#' @noRd
#' @exportPattern ^fb

fns <- ls(pattern = "^finbif")

for (fn in fns) {

  new_name <- gsub("finbif", "fb", fn)

  fn_obj <- get(fn)

  assign(new_name, fn_obj)

}

rm(fn, fns, new_name, fn_obj)
