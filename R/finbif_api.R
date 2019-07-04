# as.data.frame methods --------------------------------------------------------

#' Coerce `finbif_api*` object to a `data.frame`
#'
#' Converts the result of a FinBIF query to a `data.frame`.
#'
#' @param x A `finbif_api*` object.
#' @param ... Additional arguments. Not used.
#' @return A `data.frame`.
#' @examples \dontrun{
#'
#' # Download the latest records from FinBIF and
#' # convert to a `data.frame`
#' resp <- finbif_records()
#' df <- as.data.frame(resp)
#' }
#' @export
as.data.frame.finbif_api <- function(x, ...) {
  df <- lapply(
    x[["content"]][["results"]],
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

# print methods ----------------------------------------------------------------

#' @noRd
#' @importFrom utils str
#' @export
print.finbif_api <- function(x, ...) {
  cat("<FinBIF ", x$path, ">\n", sep = "")
  utils::str(x[["content"]])
  invisible(x)
}

#' @noRd
#' @export
print.finbif_api_list <- function(x, ...) {
  cat("<FinBIF ", x[[1]][["path"]], ">\n", sep = "")
  utils::str(x[[1]][["content"]])
  invisible(x)
}

#' @noRd
#' @export
print.finbif_taxa <- function(x, ...) {
  ranks <- names(x)
  nms   <- names(unlist(unname(x)))
  padl  <- if (is.null(ranks)) 0L else max(nchar(ranks)) + 2L
  padr  <- if (is.null(nms)) 0L else max(nchar(nms))
  unlist
  for (i in seq_along(x)) {
    rank <- ranks[[i]]
    for (j in seq_along(x[[i]])) {
      taxon <- x[[i]][j]
      nm <- names(taxon)
      nm <- ifelse(is.null(nm), "", nm)
      cat(
        sprintf(
          paste0("[%-", padl, "s"), ifelse(is.null(rank), "", paste0(rank, ": "))
        ),
        sprintf(paste0("%-", padr, "s] "), nm),
        ifelse(is.na(taxon), "Not found\n", sprintf("ID: %s\n", taxon)),
        sep = ""
      )
    }
  }
  invisible(x)
}

#' @noRd
#' @export
print.finbif_occ <- function(x, ...) {
  nrec_dnld <- attr(x, "nrec_dnld")
  nrec_avl  <- attr(x, "nrec_avl")
  ncols     <- ncol(x)
  nrows     <- nrow(x)
  dsply_nr  <- min(10, nrows)
  dsply_nc  <- min(8, ncols)
  cat(
    "Records downloaded: ", nrec_dnld, "\n",
    "Records available: ", nrec_avl, "\n",
    "A data.frame [", nrows, " x ", ncols, "]\n",
    sep = ""
  )
  df <- x[seq_len(dsply_nr), seq_len(dsply_nc)]
  for (i in names(df)) {
    type <- field_translations[
      field_translations[["translated_field"]] == i, "type"
    ]
    if (type == "uri") {
      df[[i]] <- gsub("^http:\\/\\/tun\\.fi\\/[A-Z]{2}\\.", "", df[[i]])
    }
  }
  print.data.frame(df)
  cat(
    "...with ",
    nrows - dsply_nr,
    " more records and ",
    ncols - dsply_nc,
    " more fields:\n",
    paste0(names(x)[-seq_len(dsply_nc)], c(rep(", ", 6), ",\n")),
    sep = ""
  )
  invisible(x)
}

# Utils ------------------------------------------------------------------------

#' @noRd
reduce_merge <- function(df) Reduce(function(x, y) merge(x, y, all = TRUE), df)
