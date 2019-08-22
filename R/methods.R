# as.data.frame methods --------------------------------------------------------

#' Coerce a `finbif_records*` object to a `data.frame`
#'
#' Converts the result of a FinBIF query to a `data.frame`.
#'
#' @param x A `finbif_records*` object.
#' @param ... Additional arguments. Not used.
#' @return A `data.frame`.
#' @examples \dontrun{
#'
#' # Download the latest records from FinBIF
#' # and convert to a `data.frame`
#' resp <- finbif_records()
#' df <- as.data.frame(resp)
#' }
#' @importFrom methods as
#' @export
as.data.frame.finbif_records <- function(x, ...) {

  df <- lapply(

    x[["content"]][["results"]],

    function(x) {

      dfx <- as.data.frame(x, stringsAsFactors = FALSE)
      nms <- gsub("\\d*$", "", names(unlist(x)))
      colnames(dfx) <- nms
      unms <- unique(nms)
      ans <- dfx[unms]

      # Some vars return values that are not atomic (e.g., multiple observers)
      for (nm in unms) {
        if (!var_names[nm, "unique"]) {
          el <- unlist(dfx[nms == nm])
          ans[[nm]] <- NULL
          ans[[nm]][[1L]] <- unname(el)
        }
      }

      ans

    }

  )

  # Sometimes there are, and are supposed to be, duplicate rows
  df <- lapply(
    seq_along(df),
    function(x) {
      df[[x]][["ind"]] <- x
      df[[x]]
    }
  )
  df <- reduce_merge(df)
  df[["ind"]] <- NULL

  # If list column is missing data for a record it may be NULL
  for (col in names(df))
   if (!var_names[col, "unique"])
     df[[col]] <- ifelse(
       vapply(df[[col]], is.null, logical(1)),
       list(methods::as(NA, var_names[col, "type"])),
       as.list(df[[col]])
     )

  structure(
    df,
    url = x[["response"]][["url"]],
    time = x[["response"]][["date"]]
  )

}

#' @rdname as.data.frame.finbif_records
#' @export
as.data.frame.finbif_records_list <- function(x, ...) {

  df <- lapply(x, as.data.frame)
  # Sometimes there are duplicate rows

  df <- lapply(
    seq_along(df),
    function(x) {
      # Sometimes no data are returned and df has zero rows
      df[[x]][["ind"]] <- if (length(df[[x]])) paste(x, row.names(df[[x]]))
      df[[x]]
    }
  )

  # Attributes would be lost after merging
  url  <- do.call(c, lapply(df, attr, "url", TRUE))
  time <- do.call(c, lapply(df, attr, "time", TRUE))

  df <- reduce_merge(df)
  df[["ind"]] <- NULL

  structure(df, url = url, time = time)

}

# print methods ----------------------------------------------------------------

#' @noRd
#' @importFrom utils str
#' @export
print.finbif_api <- function(x, ...) {
  cat("<FinBIF ", x[["path"]], ">\n", sep = "")
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
  dsply_nr  <- min(10L, nrows)
  dsply_nc  <- min(5L, ncols)

  if (length(nrec_dnld)) cat("Records downloaded: ", nrec_dnld, "\n", sep = "")
  if (length(nrec_avl)) cat("Records available: ", nrec_avl, "\n", sep = "")
  cat("A data.frame [", nrows, " x ", ncols, "]\n", sep = "")

  dsply_cols <-
    c("scientific_name", "abundance", "lat_wgs84", "lon_wgs84", "date_time")
  dsply_cols <- which(names(x) %in% dsply_cols)
  dsply_cols <- utils::head(union(dsply_cols, seq_len(dsply_nc)), dsply_nc)

  df <- x[seq_len(dsply_nr), dsply_cols, drop = FALSE]

  # Some scientific names are very long
  sn <- df[["scientific_name"]]
  snlng <- nchar(sn) > 20L
  if (any(snlng)) {
    df[["scientific_name"]] <-
      ifelse(snlng, sprintf("%s\u2026", substr(sn, 1L, 19L)), sn)
  }


  # Some vars have data in the form of URIs where the protocol and domain don't
  # convey useful information
  for (i in names(df)) {
    class <- var_names[var_names[["translated_var"]] == i, "class"]
    if (class == "uri") {
      df[[i]] <- gsub("^http:\\/\\/tun\\.fi\\/[A-Z]{2}\\.", "", df[[i]])
    }
  }

  print.data.frame(df)

  extra_rows <- nrows - dsply_nr
  extra_cols <- ncols - dsply_nc

  if (extra_rows == 0L && extra_cols == 0L) return(invisible(x))
  cat(
    "...with ", extra_rows, " more record", ifelse(extra_rows == 1L, "", "s"),
    sep = ""
  )

  if (extra_cols == 0L) {
    cat("\n")
    return(invisible(x))
  }

  cat(
    " and ", extra_cols, " more variable", ifelse(extra_cols == 1L, "", "s"),
    ":\n", sep = ""
  )

  # Can't tell in advance what the var names will be
  i <- 1L
  extra_names <- names(x)[-dsply_cols]
  cat(extra_names[[i]])
  nchars <- nchar(extra_names[[i]]) + 2L

  for (i in seq_along(extra_names)[-1L]) {
    nchars_ <- nchar(extra_names[[i]]) + 2L
    nchars  <- nchars + nchars_
    if (nchars > 70L) {
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
