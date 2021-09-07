.onLoad <- function(libname, pkgname) { # nolint
  op <- options()
  op_finbif <- list(
    # There are (or will be) multiple versions and locations of the FinBIF API
    finbif_api_url = "https://api.laji.fi",
    finbif_dl_url = "https://dw.laji.fi/download",
    finbif_api_version = "v0",
    finbif_warehouse_query = "warehouse/query/",
    finbif_allow_query = TRUE,
    finbif_rate_limit = 1,
    finbif_max_queries = 2000L,
    finbif_max_page_size = 1000L,
    finbif_use_cache = TRUE,
    finbif_tz = Sys.timezone(),
    finbif_locale = get_locale()
  )
  toset <- !(names(op_finbif) %in% names(op))
  if (any(toset)) options(op_finbif[toset])
  invisible()
}

#' @exportPattern ^fb
for (fn in ls(pattern = "^finbif")) assign(gsub("finbif", "fb", fn), get(fn))
rm(fn)
