#' Coerce `finbif_api` object to a `data.frame`
#'
#' Converts the result of a FinBIF query to a `data.frame`.
#'
#' @param x A `finbif_api` object.
#' @param ... Additional arguments. Not used.
#' @return `data.frame`
#' @export
as.data.frame.finbif_api <- function(x, ...) {
  df <- lapply(
    x$content$results,
    function(x) {
      dfx <- as.data.frame(x, stringsAsFactors = FALSE)
      colnames(dfx) <- names(unlist(x))
      dfx
    }
  )
  Reduce(function(x, y) merge(x, y, all = TRUE), df)
}

#' @importFrom utils str
#' @export
print.finbif_api <- function(x, ...) {
  cat("<FinBIF ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}
