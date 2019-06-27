#' Coerce `finbif_api*` object to a `data.frame`
#'
#' Converts the result of a FinBIF query to a `data.frame`.
#'
#' @param x A `finbif_api*` object.
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
  reduce_merge(df)
}

#' @rdname as.data.frame.finbif_api
#' @export
as.data.frame.finbif_api_list <- function(x, ...) {
  df <- lapply(x, as.data.frame)
  reduce_merge(df)
}

#' @importFrom utils str
#' @export
print.finbif_api <- function(x, ...) {
  cat("<FinBIF ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}

#' @export
print.finbif_api_list <- function(x, ...) {
  cat("<FinBIF ", x[[1]]$path, ">\n", sep = "")
  utils::str(x[[1]]$content)
  invisible(x)
}

reduce_merge <- function(df) Reduce(function(x, y) merge(x, y, all = TRUE), df)
