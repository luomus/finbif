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

reduce_merge <- function(df) Reduce(function(x, y) merge(x, y, all = TRUE), df)

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

#' @export
print.finbif_taxa <- function(x, ...) {
  ranks <- names(x)
  padl <- max(nchar(ranks)) + 1L
  padr <- max(nchar(names(unlist(unname(x)))))
  unlist
  for (i in seq_along(x)) {
    rank <- ranks[[i]]
    for (j in seq_along(x[[i]])) {
      taxon <- x[[i]][j]
      cat(
        sprintf(paste0("[%-", padl, "s"), paste0(rank, ":")),
        sprintf(paste0("%-", padr, "s]"), names(taxon)),
        ifelse(is.na(taxon), "Not found", sprintf("ID: %s\n", taxon))
      )
    }
  }
  invisible(x)
}

