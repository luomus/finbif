.onLoad <- function(libname, pkgname) {
  op <- options()
  op_finbif <- list(
    # There are (or will be) multiple versions and locations of the FinBIF API
    finbif_api_url = "api.laji.fi",
    finbif_api_version = "v0",
    finbif_use_cache = TRUE
  )
  toset <- !(names(op_finbif) %in% names(op))
  if (any(toset)) options(op_finbif[toset])
  invisible()
}
