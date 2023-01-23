# as.data.frame methods --------------------------------------------------------

#' Coerce a `finbif_records*` object to a `data.frame`
#'
#' Converts the result of a FinBIF query to a `data.frame`.
#'
#' @param x A `finbif_records*` object.
#' @param ... Additional arguments. Not used.
#' @param locale Character. A locale to use for columns with localised data.
#' @param quiet Logical. If `TRUE` (default) suppress progress indicator of
#'   conversion.
#' @return A `data.frame`.
#' @examples \dontrun{
#'
#' # Download the latest records from FinBIF
#' # and convert to a `data.frame`
#' resp <- finbif_records()
#' df <- as.data.frame(resp)
#' }
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

as.data.frame.finbif_records <- function(
  x,
  ...,
  locale = getOption("finbif_locale")
) {

  cols <- attr(x, "select")

  resp <- x[["response"]]

  url  <- resp[["url"]]

  time <- resp[["date"]]

  aggregation <- attr(x, "aggregate")

  aggregated <- !identical(aggregation, "none")

  x <- x[["content"]]

  x <- x[["results"]]

  if (aggregated) {

    aggregations <- c(
      records = "count",
      species = "speciesCount",
      taxa = "taxonCount",
      events = "count",
      documents = "count"
    )

    aggregations <- aggregations[aggregation]

    aggregation_nms <- names(aggregations)

    counts <- list()

    aggregations_seq <- seq_along(aggregations)

    for (i in aggregations_seq) {

      aggregation_i <- aggregations[[i]]

      aggregation_nm <- aggregation_nms[[i]]

      count <- vapply(x, getElement, 0L, aggregation_i)

      counts[[aggregation_nm]] <- count

    }

    x <- lapply(x, getElement, "aggregateBy")

  }

  lst <- list()

  for (col in cols) {

    type  <- var_names[[col, "type"]]

    type_na <- cast_to_type(NA, type)

    single <- var_names[[col, "single"]]

    localised <- var_names[[col, "localised"]]

    if (aggregated) {

      ans <- vapply(x, getElement, NA_character_, col)

      ans_empty <- ans == ""

      ans <- ifelse(ans_empty, NA_character_, ans)

      if (localised) {

        ans <- localise_labels(ans, col, var_names, locale)

      }

      ans <- cast_to_type(ans, type)

    } else {

      col_els <- strsplit(col, "\\.")

      col_els <- col_els[[1L]]

      if (single) {

        ans <- vapply(x, get_el_recurse, type_na, col_els, type)

        if (localised) {

          ans <- localise_labels(ans, col, var_names, locale)

        }

      } else {

        ans <- lapply(x, get_el_recurse, col_els, type)

        ans <- lapply(ans, unlist)

        if (localised) {

          langs <- lapply(ans, names)

          langs <- unlist(langs)

          has_locale <- langs %in% supported_langs

          has_locale <- any(has_locale)

          if (has_locale) {

            ans <- vapply(ans, with_locale, type_na, locale)

          } else {

            ans <- lapply(ans, localise_labels, col, var_names, locale)

          }

        }

      }

    }

    lst[[col]] <- ans

  }

  names(lst) <- cols

  single_col <- var_names[cols, "single"]

  cols_split <- split(cols, single_col)

  cols_single <- cols_split[["TRUE"]]

  lst_single <- lst[cols_single, drop = FALSE]

  df <- as.data.frame(lst_single, stringsAsFactors = FALSE)

  cols_list <- cols_split[["FALSE"]]

  lst_list <- lst[cols_list, drop = FALSE]

  df[cols_list] <- lst_list

  if (aggregated) {

    aggregation_cols <- paste0("n_", aggregation)

    cols <- c(cols, aggregation_cols)

    aggregation_sq <- seq_along(aggregation)

    for (i in aggregation_sq) {

      counts_i <- counts[[i]]

      aggregation_col_i <- aggregation_cols[[i]]

      df[[aggregation_col_i]] <- counts_i

    }

  }

  df <- df[cols]

  structure(df, url = url, time = time)

}

#' @noRd
localise_labels <- function(x, col, var_names, locale) {

  labels <- get(var_names[[col, "translated_var"]])

  label_col <- max(c(1L, which(names(labels) == paste0("name_", locale))))

  localised_labels <- ifelse(is.na(x), x, labels[x, label_col])

  ifelse(is.na(localised_labels), x, localised_labels)

}

#' @rdname as.data.frame.finbif_records
#' @export
as.data.frame.finbif_records_list <- function(
  x, ..., locale = getOption("finbif_locale"), quiet = TRUE
) {

  n <- length(x)
  if (!quiet) {
    pb <- utils::txtProgressBar(0L, n, style = 3L)
    on.exit(close(pb))
  }

  df <- lapply(
    seq_len(n),
    function(i) {
      if (!quiet) utils::setTxtProgressBar(pb, i)
      x_df <- attr(x[[i]], "df")
      if (is.null(x_df)) {
        as.data.frame(x[[i]], locale = locale)
      } else {
        x_df
      }
    }
  )

  url  <- do.call(c, lapply(df, attr, "url", TRUE))
  time <- do.call(c, lapply(df, attr, "time", TRUE))

  df <- do.call(rbind, df)

  record_id <- df[["unit.unitId"]]

  if (inherits(x, "finbif_records_sample_list")) {
    records <- if (attr(x, "cache")) {
      sample_with_seed(nrow(df), nrow(df), gen_seed(x))
    } else {
      sample.int(nrow(df))
    }
    df <- df[records, ]
  }

  if (!attr(x, "record_id")) df[["unit.unitId"]] <- NULL

  structure(df, url = url, time = time, record_id = record_id)

}

# accessor methods -------------------------------------------------------------

#' @noRd
#' @export
`[.finbif_occ` <- function(x, i, j, drop = FALSE) {

  mdrop  <- missing(drop)
  n_args <- nargs() - !mdrop
  has_i  <- !missing(i)
  has_j  <- !missing(j)

  rows <- seq_len(nrow(x))
  cols <- seq_len(ncol(x))

  if (n_args < 3L) {
    if (has_i) cols <- i
  } else {
    if (has_j) cols <- j
    if (has_i) {
      if (is.logical(i)) i <- which(rep_len(i, nrow(x)))
      rows <- i
    }
  }

  ans <- if (has_j) NextMethod("[", drop = drop) else NextMethod("[")
  ans <- structure(
    as.data.frame(ans, stringsAsFactors = FALSE), class = class(x)
  )
  attr <- attributes(x)
  attr[["row.names"]] <- as.integer(rows)
  attr[["record_id"]] <- attr[["record_id"]][as.integer(rows)]
  mostattributes(ans) <- attr
  names(ans) <- if (is.character(cols)) cols else names(x)[cols]
  ans
}

# rbind methods ----------------------------------------------------------------

#' @noRd
#' @export
rbind.finbif_occ <- function(...) {

  ans <- rbind.data.frame(...)

  l <- list(...)

  for (i in c("nrec_dnld", "nrec_avl", "url", "time", "record_id")) {
    attr(ans, i) <- unlist(lapply(l, attr, i))
  }

  ans

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
print.finbif_taxa_list <- function(x, ...) {
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
#' @export
print.finbif_metadata_df <- function(x, ..., right = FALSE) {
  df <- x
  sl <- max(9L, getOption("width") / ncol(df))
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- truncate_string(df[[col]], max(sl, nchar(col)))
    }
  }
  print.data.frame(df, ..., right = right)
  invisible(x)
}

#' @noRd
#' @importFrom utils hasName head
#' @export
print.finbif_occ <- function(x, ...) {

  dwdth <- getOption("width")

  ncols     <- ncol(x)
  nrows     <- nrow(x)
  dsply_nr  <- min(10L, nrows)

  records_msg(x, dwdth, nrec_dnld = "downloaded", nrec_avl = "available")

  cat("A data.frame [", nrows, " x ", ncols, "]\n", sep = "")

  df <- x[seq_len(dsply_nr), , drop = FALSE]

  colname_widths <- vapply(names(df), nchar, integer(1L))
  widths <- colname_widths

  if (nrows) {
    df <- format_cols(df, colname_widths)
    widths <- apply(df, 2L, nchar)
    dim(widths) <- dim(df)
    # Printed NA values are four characters wide, "<NA>"
    widths[is.na(widths)] <- 4L
    widths <- apply(widths, 2L, max, na.rm = TRUE)
    widths <- pmax(colname_widths, widths)
  }

  dsply_nc <- 0L
  cumulative_width <- if (dsply_nr > 9L) 2L else 1L

  for (i in widths) {
    ind <- dsply_nc + 1L
    cumulative_width <- cumulative_width + 1L + widths[[ind]]
    if (cumulative_width > dwdth) break
    dsply_nc <- ind
  }

  dsply_cols <- seq_len(dsply_nc)

  df <- df[, dsply_cols]

  print.data.frame(df)

  extra_rows <- nrows - dsply_nr
  extra_cols <- ncols - dsply_nc

  print_extras(x, extra_rows, extra_cols, dsply_cols)

  invisible(x)

}

#' @noRd
records_msg <- function(x, width, ...) {
  l <- list(...)

  for (i in seq_along(l)) {

    n <- attr(x, names(l)[[i]])

    if (!identical(length(n), 0L)) {

      suf <- paste0("Records ", l[[i]], ": ")
      n <- truncate_string(paste(n, collapse = " + "), width - nchar(suf) - 2L)
      cat(suf, n, "\n", sep = "")

    }

  }

}

#' @noRd
format_cols <- function(df, colname_widths) {

  for (i in seq_along(df)) {

    class <- col_class(df[[i]])

    single <- !is.list(df[[i]])

    # Variables may not necessarily be in the var_names object
    if (length(class)) {
      if (!single) {
        df[[i]] <- vapply(
          df[[i]],
          function(x) length(Filter(isFALSE, is.na(x))), integer(1L)
        )
        df[[i]] <- paste0(df[[i]], " element", ifelse(df[[i]] == 1L, "", "s"))
      } else {
        if (class == "uri") df[[i]] <- truncate_string_to_unique(df[[i]])
        if (class %in% c("uri", "character")) {
          df[[i]] <- truncate_string(df[[i]])
        }
        if (class %in% c("double", "integer")) {
          df[[i]] <- formatC(
            df[[i]],
            colname_widths[[i]] - 2L, colname_widths[[i]],
            flag = "- "
          )
        }
      }
    }

  }

  df

}

#' @noRd
col_class <- function(x) {

  class <-  "character"

  if (any(grepl("^http", x))) {

    class <- "uri"
  }

  if (is.numeric(x)) {

    class <- "double"

  }

  if (is.integer(x)) {

    class <- "integer"

  }

  class

}

#' @noRd
print_extras <- function(x, extra_rows, extra_cols, dsply_cols) {

  if (extra_rows == 0L && extra_cols == 0L) return(NULL)

  cat(
    "...with ", extra_rows, " more record", if (extra_rows == 1L) "" else "s",
    sep = ""
  )

  if (extra_cols == 0L) {
    cat("\n")
    return(NULL)
  }

  cat(
    " and ", extra_cols, " more variable", if (extra_cols == 1L) "" else "s",
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
    if (nchars > getOption("width")) {
      cat(",\n")
      nchars <- nchars_
    } else {
      cat(", ")
    }
    cat(extra_names[[i]])
  }

  cat("\n")
  NULL
}

# plot methods ----------------------------------------------------------------

#' @noRd
#' @importFrom graphics grid plot.default
#' @export
plot.finbif_occ <- function(
  x, ..., xlab = "Longitude", ylab = "Latitude",
  panel.first = grid(lwd = 2), # nolint
  asp = 1 / cos(mean(range(x["lat_wgs84"])) * pi / 180), pch = 19, cex = .5,
  las = 1
) {
  stopifnot(exists("lon_wgs84", x) && exists("lat_wgs84", x))
  graphics::plot.default(
    x[c("lon_wgs84", "lat_wgs84")], ..., xlab = xlab, ylab = ylab,
    panel.first = panel.first, asp = asp, cex = .5, las = 1, pch = pch
  )
}
