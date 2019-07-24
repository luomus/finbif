.onLoad <- function(libname, pkgname) {
  op <- options()
  op_finbif <- list(finbif_use_cache = TRUE)
  toset <- !(names(op_finbif) %in% names(op))
  if (any(toset)) options(op_finbif[toset])
  invisible()
}
