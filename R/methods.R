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
#'
#' }
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

  attr(x, "select") <- cols

  attr(x, "locale") <- locale

  attr(x, "aggregated") <- aggregated

  lst <- process_cols(x)

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

process_cols <- function(x) {

  cols <- attr(x, "select")

  locale <- attr(x, "locale")

  aggregated <- attr(x, "aggregated")

  col_list <- list()

  for (col in cols) {

    type  <- var_names[[col, "type"]]

    type_na <- cast_to_type(NA, type)

    single <- var_names[[col, "single"]]

    localised <- var_names[[col, "localised"]]

    labels_obj <- list(col = col, var_names = var_names, locale = locale)

    if (aggregated) {

      ans <- vapply(x, getElement, NA_character_, col)

      ans_empty <- ans == ""

      ans <- ifelse(ans_empty, NA_character_, ans)

      if (localised) {

        labels_obj[["labels"]] <- ans

        ans <- localise_labels(labels_obj)

      }

      ans <- cast_to_type(ans, type)

    } else {

      col_els <- strsplit(col, "\\.")

      col_els <- col_els[[1L]]

      if (single) {

        ans <- vapply(x, get_el_recurse, type_na, col_els, type)

        if (localised) {

          labels_obj[["labels"]] <- ans

          ans <- localise_labels(labels_obj)

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

            ans_seq <- seq_along(ans)

            for (i in ans_seq) {

              labels <- ans[[i]]

              labels_obj[["labels"]] <- labels

              labels <- localise_labels(labels_obj)

              ans[[i]] <- labels

            }

          }

        }

      }

    }

    col_list[[col]] <- ans

  }

  col_list

}

#' @noRd

localise_labels <- function(labels_obj) {

  obj_labels <- labels_obj[["labels"]]

  col <- labels_obj[["col"]]

  var_names <- labels_obj[["var_names"]]

  locale <- labels_obj[["locale"]]

  translated_var <- var_names[[col, "translated_var"]]

  new_labels <- get(translated_var)

  locale_col <- paste0("name_", locale)

  new_label_names <- names(new_labels)

  label_col <- which(new_label_names == locale_col)

  label_col <- max(1L, label_col)

  labels_na <- is.na(obj_labels)

  localised_labels <- new_labels[obj_labels, label_col]

  localised_labels <- ifelse(labels_na, obj_labels, localised_labels)

  localised_labels_na <- is.na(localised_labels)

  ifelse(localised_labels_na, obj_labels, localised_labels)

}

#' @rdname as.data.frame.finbif_records
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

as.data.frame.finbif_records_list <- function(
  x,
  ...,
  locale = getOption("finbif_locale"),
  quiet = TRUE
) {

  n <- length(x)

  if (!quiet) {

    pb <- utils::txtProgressBar(0L, n, style = 3L)

    on.exit({

      close(pb)

    })

  }

  df <- list()

  sq <- seq_len(n)

  for (i in sq) {

    if (!quiet) {

      utils::setTxtProgressBar(pb, i)

    }

    xi <- x[[i]]

    dfi <- attr(xi, "df")

    df_null <- is.null(dfi)

    if (df_null) {

      dfi <- as.data.frame(xi, locale = locale)

    }

    df[[i]] <- dfi

  }

  url <- vapply(df, attr, "", "url", TRUE)

  time <- lapply(df, attr, "time", TRUE)

  time <- do.call(c, time)

  df <- do.call(rbind, df)

  record_id <- df[["unit.unitId"]]

  is_sample_list <- inherits(x, "finbif_records_sample_list")

  if (is_sample_list) {

    nrows <- nrow(df)

    records <- sample.int(nrows)

    is_cached <- attr(x, "cache")

    if (is_cached) {

      seed <- gen_seed(x)

      records <- sample_with_seed(nrows, nrows, seed)

    }

    df <- df[records, ]

  }

  no_id <- !attr(x, "record_id")

  if (no_id) {

    df[["unit.unitId"]] <- NULL

  }

  structure(df, url = url, time = time, record_id = record_id)

}

# accessor methods -------------------------------------------------------------

#' @noRd
#' @export

`[.finbif_occ` <- function(
  x,
  i,
  j,
  drop = FALSE
) {

  n_args <- nargs()

  has_drop <- !missing(drop)

  if (has_drop) {

    n_args <- n_args - 1L

  }

  has_i  <- !missing(i)

  has_j  <- !missing(j)

  nrows <- nrow(x)

  ncols <- ncol(x)

  rows <- seq_len(nrows)

  cols <- seq_len(ncols)

  lt3 <- n_args < 3L

  if (lt3) {

    if (has_i) {

      cols <- i

    }

  } else {

    if (has_j) {

      cols <- j

    }

    if (has_i) {

      i_is_bool <- is.logical(i)

      if (i_is_bool) {

        i <- rep_len(i, nrows)

        i <- which(i)

      }

      rows <- i

    }

  }

  if (has_j) {

    ans <- NextMethod("[", drop = drop)

  } else {

    ans <- NextMethod("[")

  }

  ans <- as.data.frame(ans, stringsAsFactors = FALSE)

  class <- class(x)

  ans <- structure(ans, class = class)

  attr <- attributes(x)

  rows <- as.integer(rows)

  attr[["row.names"]] <- rows

  id <- attr[["record_id"]]

  id <- id[rows]

  attr[["record_id"]] <- id

  mostattributes(ans) <- attr

  nms <- names(x)

  cols_not_nms <- !is.character(cols)

  if (cols_not_nms) {

    cols <- nms[cols]

  }

  names(ans) <- cols

  ans

}

# rbind methods ----------------------------------------------------------------

#' @noRd
#' @export

rbind.finbif_occ <- function(...) {

  ans <- rbind.data.frame(...)

  l <- list(...)

  attrs <- c("nrec_dnld", "nrec_avl", "url", "time", "record_id")

  for (i in attrs) {

    a <- lapply(l, attr, i)

    a <- unlist(a)

    attr(ans, i) <- a

  }

  ans

}

# print methods ----------------------------------------------------------------

#' @noRd
#' @export

print.finbif_api <- function(
  x,
  ...
) {

  print_finbif_api(x)

}

#' @noRd
#' @export

print.finbif_api_list <- function(
  x,
  ...
) {

  x <- x[[1L]]

  print_finbif_api(x)

}

#' @noRd
#' @importFrom utils str

print_finbif_api <- function(x) {

  path <- x[["path"]]

  content <- x[["content"]]

  cat("<FinBIF ", path, ">\n", sep = "")

  utils::str(content)

  invisible(x)

}

#' @noRd
#' @export

print.finbif_taxa_list <- function(
  x,
  ...
) {

  ranks <- names(x)

  nms <- unname(x)

  nms <- unlist(x)

  nms <- names(x)

  has_ranks <- !is.null(ranks)

  padl <- 0L

  if (has_ranks) {

    padl <- nchar(ranks)

    padl <- max(padl)

    padl <- padl + 2L

  }

  padl <- paste0("[%-", padl, "s")

  padr <- 0L

  has_nms <- !is.null(nms)

  if (has_nms) {

    padr <- nchar(nms)

    padr <- max(padr)

  }

  padr <- paste0("%-", padr, "s] ")

  sq <- seq_along(x)

  for (i in sq) {

    rank <- ranks[[i]]

    rank_is_null <- is.null(rank)

    rank <- paste0(rank, ": ")

    if (rank_is_null) {

      rank <- ""

    }

    rank <- sprintf(padl, rank)

    xi <- x[[i]]

    sqi <- seq_along(xi)

    for (j in sqi) {

      taxon <- xi[j]

      nm <- names(taxon)

      nm <- sprintf(padr, nm)

      taxon_is_na <- is.na(taxon)

      taxon <- sprintf("ID: %s\n", taxon)

      if (taxon_is_na) {

        taxon <- "Not found\n"

      }

      cat(rank, nm, taxon, sep = "")

    }

  }

  invisible(x)

}

#' @noRd
#' @export

print.finbif_metadata_df <- function(
  x,
  ...,
  right = FALSE
) {

  df <- x

  width <- getOption("width")

  ncols <- ncol(df)

  sl <- width / ncols

  sl <- max(9L, sl)

  colnames <- names(df)

  for (col in colnames) {

    df_col <- df[[col]]

    col_is_char <- is.character(df_col)

    if (col_is_char) {

      n <- nchar(col)

      n <- max(sl, n)

      df_col <- truncate_string(df_col, n)

      df[[col]] <- df_col

    }

  }

  print.data.frame(df, ..., right = right)

  invisible(x)

}

#' @noRd
#' @export

print.finbif_occ <- function(
  x,
  ...
) {

  dwdth <- getOption("width")

  ncols <- ncol(x)

  nrows <- nrow(x)

  dsply_nr  <- min(10L, nrows)

  msg_obj <- list(x, width = dwdth, dnld = "downloaded", avl = "available")

  records_msg(msg_obj)

  cat("A data.frame [", nrows, " x ", ncols, "]\n", sep = "")

  sq <- seq_len(dsply_nr)

  df <- x[sq, , drop = FALSE]

  colnames <- names(df)

  colname_widths <- vapply(colnames, nchar, 0L)

  widths <- colname_widths

  has_rows <- nrows > 0L

  if (has_rows) {

    obj <- list(df = df, colname_widths = colname_widths)

    df <- format_cols(obj)

    widths <- apply(df, 2L, nchar)

    dim(widths) <- dim(df)

    na_widths <- is.na(widths)

    # Printed NA values are four characters wide, "<NA>"
    widths[na_widths] <- 4L

    widths <- apply(widths, 2L, max, na.rm = TRUE)

    widths <- pmax(colname_widths, widths)

  }

  dsply_nc <- 0L

  gt_9_rows <- dsply_nr > 9L

  cumulative_width <- 1L

  if (gt_9_rows) {

    cumulative_width <-  2L

  }

  for (width in widths) {

    i <- dsply_nc + 1L

    width_i <- widths[[i]]

    cumulative_width <- cumulative_width + 1L + width_i

    end_loop <- cumulative_width > dwdth

    if (end_loop) {

      break

    }

    dsply_nc <- i

  }

  dsply_cols <- seq_len(dsply_nc)

  df <- df[, dsply_cols]

  print.data.frame(df)

  extra_rows <- nrows - dsply_nr

  extra_cols <- ncols - dsply_nc

  extras_obj <- list(x, rows = extra_rows, cols = extra_cols, dc = dsply_cols)

  print_extras(extras_obj)

  invisible(x)

}

#' @noRd

records_msg <- function(obj) {

  x <- obj[[1L]]

  width <- obj[["width"]]

  dnld <- obj[["dnld"]]

  avl <- obj[["avl"]]

  l <- list(nrec_dnld = dnld, nrec_avl = avl)

  sq <- seq_along(l)

  nms <- names(l)

  for (i in sq) {

    nm <- nms[[i]]

    x_nm <- attr(x, nm)

    nl <- length(x_nm)

    cond <- !identical(nl, 0L)

    if (cond) {

      li <- l[[i]]

      suf <- paste0("Records ", li, ": ")

      x_nm <- paste(x_nm, collapse = " + ")

      sufl <- nchar(suf)

      n <- width - sufl - 2L

      x_nm <- truncate_string(x_nm, n)

      cat(suf, x_nm, "\n", sep = "")

    }

  }

}

#' @noRd

format_cols <- function(obj) {

  df <- obj[["df"]]

  colname_widths <- obj[["colname_widths"]]

  sq <- seq_along(df)

  num_class <- c("double", "integer")

  for (i in sq) {

    dfi <- df[[i]]

    class <- col_class(dfi)

    is_list_col <- is.list(dfi)

    cl <- length(class)

    has_class <- cl > 0L

    if (has_class) {

      if (is_list_col) {

        dfi_len <- lapply(dfi, length)

        dfi_len <- unlist(dfi_len)

        dfi_na <- lapply(dfi, is.na)

        dfi_na <- lapply(dfi_na, sum)

        dfi_na <- unlist(dfi_na)

        dfi <- dfi_len - dfi_na

        dfi_singular <- dfi == 1L

        suffix <- ifelse(dfi_singular, "", "s")

        dfi <- paste0(dfi, " element", suffix)

      } else {

        is_uri <- identical(class, "uri")

        if (is_uri) {

          dfi <- truncate_string_to_unique(dfi)

        }

        is_char <- identical(class, "character")

        is_char <- is_char || is_uri

        if (is_char) {

          dfi <- truncate_string(dfi)

        }

        is_num <- class %in% num_class

        if (is_num) {

          cwi <- colname_widths[[i]]

          digits <- cwi - 2L

          dfi <- formatC(dfi, digits, cwi, flag = "- ")

        }

      }

    }

    df[[i]] <- dfi

  }

  df

}

#' @noRd

col_class <- function(x) {

  class <-  "character"

  uris <- grepl("^http", x)

  has_uri <- any(uris)

  if (has_uri) {

    class <- "uri"
  }

  has_num <- is.numeric(x)

  if (has_num) {

    class <- "double"

  }

  has_int <- is.integer(x)

  if (has_int) {

    class <- "integer"

  }

  class

}

#' @noRd
print_extras <- function(obj) {

  x <- obj[[1L]]

  extra_rows <- obj[["rows"]]

  extra_cols <- obj[["cols"]]

  dsply_cols <- obj[["dc"]]

  no_cols <- identical(extra_cols, 0L)

  no_data <- no_cols && identical(extra_rows, 0L)

  if (no_data) {

    return(NULL)

  }

  suffix <- ""

  more_rows <- extra_rows > 1L

  if (more_rows) {

    suffix <- "s"

  }

  cat("...with ", extra_rows, " more record", suffix, sep = "")

  if (no_cols) {

    cat("\n")

    return(NULL)

  }

  suffix <- ""

  more_cols <- extra_cols > 1L

  if (more_cols) {

    suffix <- "s"

  }

  cat(" and ", extra_cols, " more variable", suffix, ":\n", sep = "")

  i <- 1L

  extra_names <- names(x)

  extra_names <- extra_names[-dsply_cols]

  extra_name_i <- extra_names[[i]]

  cat(extra_name_i)

  nchars <- nchar(extra_name_i)

  nchars <- nchars + 2L

  sq <- seq_along(extra_names)

  sq <- sq[-1L]

  width <- getOption("width")

  for (i in sq) {

    extra_name_i <- extra_names[[i]]

    nchars_next <- nchar(extra_name_i)

    nchars_next <- nchars_next + 2L

    nchars  <- nchars + nchars_next

    too_many_chars <- nchars > width

    sep <- ", "

    if (too_many_chars) {

      sep <- ",\n"

      nchars <- nchars_next

    }

    cat(sep, extra_name_i, sep = "")

  }

  cat("\n")

  NULL

}

# plot methods ----------------------------------------------------------------

#' @noRd
#' @importFrom graphics grid plot.default
#' @export

plot.finbif_occ <- function(
  x,
  ...,
  xlab = "Longitude",
  ylab = "Latitude",
  panel.first = grid(lwd = 2), # nolint
  asp = 1 / cos(mean(range(x["lat_wgs84"])) * pi / 180),
  pch = 19,
  cex = .5,
  las = 1
) {

  has_lon <- exists("lon_wgs84", x)

  has_lat <- exists("lat_wgs84", x)

  has_coords <- has_lon && has_lat

  stopifnot("Missing coordinates" = has_coords)

  coord_nms <- c("lon_wgs84", "lat_wgs84")

  coords <- x[coord_nms]

  graphics::plot.default(
    coords,
    ...,
    xlab = xlab,
    ylab = ylab,
    panel.first = panel.first,
    asp = asp,
    cex = .5,
    las = 1,
    pch = pch
  )

}
