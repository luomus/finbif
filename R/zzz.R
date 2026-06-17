#' @noRd
.onLoad <- function(libname, pkgname) { # nocov start
  op_finbif <- list(
    finbif_api_url = "https://api.laji.fi",
    finbif_dl_url = "https://dw.laji.fi/download",
    finbif_api_version = 1,
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
    finbif_use_cache_metadata = TRUE,
    finbif_timeout_offset = 0,
    finbif_tz = Sys.timezone(),
    finbif_locale = get_locale(),
    finbif_use_dwc = TRUE,
    finbif_use_all_collections = FALSE
  )
  op <- options()
  toset <- !names(op_finbif) %in% names(op)

  if (any(toset)) {
    options(op_finbif[toset])
  }

  invisible(NULL)
}

#' @noRd
get_locale <- function() {
  ans <- "en"
  supported <- sysdata(list(which = "supported_langs"))
  matches <- name_chr_vec(c(unname(supported), supported))
  env <- Sys.getenv(c("LANGUAGE", "LANG"))
  collate <- Sys.getlocale("LC_COLLATE")

  for (l in c(env, collate)) {
    reg <- regexpr(".+?(?=[[:punct:]])", l, perl = TRUE)
    l <- regmatches(l, reg)

    if (isTRUE(l %in% names(matches))) {
      ans <- matches[[l]]
      break
    }

  }

  ans
} # nocov end
