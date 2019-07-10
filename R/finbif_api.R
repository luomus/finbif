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
          paste0(
            "[%-", padl, "s"), ifelse(is.null(rank), "", paste0(rank, ": ")
          )
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
#' @importFrom utils head
#' @export
print.finbif_occ <- function(x, ...) {
  nrec_dnld <- attr(x, "nrec_dnld")
  nrec_avl  <- attr(x, "nrec_avl")
  ncols     <- ncol(x)
  nrows     <- nrow(x)
  dsply_nr  <- min(10, nrows)
  dsply_nc  <- min(5, ncols)
  if (length(nrec_dnld)) cat("Records downloaded: ", nrec_dnld, "\n", sep = "")
  if (length(nrec_avl)) cat("Records available: ", nrec_avl, "\n", sep = "")
  cat("A data.frame [", nrows, " x ", ncols, "]\n", sep = "")
  dsply_cols <- c(
    "scientific_name", "abundance", "lat_wgs84", "lon_wgs84", "date_start"
  )
  dsply_cols <- which(names(x) %in% dsply_cols)
  dsply_cols <- utils::head(union(dsply_cols, seq_len(dsply_nc)), dsply_nc)
  df <- x[seq_len(dsply_nr), dsply_cols, drop = FALSE]
  for (i in names(df)) {
    type <- field_translations[
      field_translations[["translated_field"]] == i, "type"
    ]
    if (type == "uri") {
      df[[i]] <- gsub("^http:\\/\\/tun\\.fi\\/[A-Z]{2}\\.", "", df[[i]])
    }
  }
  print.data.frame(df)
  extra_rows <- nrows - dsply_nr
  extra_cols <- ncols - dsply_nc
  if (extra_rows == 0 && extra_cols == 0) return(invisible(x))
  cat("...with ", nrows - dsply_nr, " more records", sep = "")
  if (extra_cols == 0) {
    cat("\n")
    return(invisible(x))
  }
  cat(" and ", ncols - dsply_nc, " more fields:\n", sep = "")


  i <- 1L
  extra_names <- names(x)[-dsply_cols]
  cat(extra_names[[i]])
  nchars <- nchar(extra_names[[i]]) + 2L

  for (i in seq_along(extra_names)[-1L]) {
    nchars_ <- nchar(extra_names[[i]]) + 2L
    nchars  <- nchars + nchars_
    if (nchars > 60L) {
      cat(",\n")
      nchars <- nchars_
    } else {
      cat(", ")
    }
    cat(extra_names[[i]])
  }

  cat("\n")

  invisible(x)
}

# Utils ------------------------------------------------------------------------

#' @noRd
reduce_merge <- function(df) Reduce(function(x, y) merge(x, y, all = TRUE), df)
