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

  if (!missing(drop)) {

    n_args <- n_args - 1L

  }

  has_i  <- !missing(i)

  has_j  <- !missing(j)

  nrows <- nrow(x)

  ncols <- ncol(x)

  rows <- seq_len(nrows)

  cols <- seq_len(ncols)

  if (n_args < 3L) {

    if (has_i) {

      cols <- i

    }

  } else {

    if (has_j) {

      cols <- j

    }

    if (has_i) {

      if (is.logical(i)) {

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

  ans <- structure(as.data.frame(ans), class = class(x))

  attr <- attributes(x)

  rows <- as.integer(rows)

  attr[["row.names"]] <- rows

  id <- attr[["record_id"]]

  attr[["record_id"]] <- id[rows]

  mostattributes(ans) <- attr

  nms <- names(x)

  if (!is.character(cols)) {

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

print.finbif_taxa_list <- function(
  x,
  ...
) {

  ranks <- names(x)

  nms <- unname(x)

  nms <- unlist(nms)

  nms <- names(nms)

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

        is_char <- is_uri || identical(class, "character")

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
