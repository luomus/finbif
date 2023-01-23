#' @noRd

.onLoad <- function(libname, pkgname) {

  op <- options()

  op_names <- names(op)

  locale <- get_locale()

  tz <- Sys.timezone()

  op_finbif <- list(
    finbif_api_url = "https://api.laji.fi",
    finbif_dl_url = "https://dw.laji.fi/download",
    finbif_api_version = "v0",
    finbif_warehouse_query = "warehouse/query/",
    finbif_allow_query = TRUE,
    finbif_hide_progress = FALSE,
    finbif_rate_limit = 1,
    finbif_max_queries = 2000L,
    finbif_max_page_size = 1000L,
    finbif_retry_times = 3L,
    finbif_retry_pause_base = 1L,
    finbif_retry_pause_cap = 60L,
    finbif_retry_pause_min = 1L,
    finbif_use_cache = TRUE,
    finbif_tz = tz,
    finbif_locale = locale
  )

  op_finbif_names <- names(op_finbif)

  toset <- !op_finbif_names %in% op_names

  any_toset <- any(toset)

  if (any_toset) {

    op_finbif_toset <- op_finbif[toset]

    options(op_finbif_toset)

  }

  invisible()

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
