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

  for (i in c("nrec_dnld", "nrec_avl", "url", "time", "record_id")) {

    a <- lapply(l, attr, i)

    attr(ans, i) <- unlist(a)

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

  padl <- 0L

  if (!is.null(ranks)) {

    padl <- nchar(ranks)

    padl <- max(padl) + 2L

  }

  padl <- paste0("[%-", padl, "s")

  padr <- 0L

  if (!is.null(nms)) {

    padr <- nchar(nms)

    padr <- max(padr)

  }

  padr <- paste0("%-", padr, "s] ")

  for (i in seq_along(x)) {

    rank <- ranks[[i]]

    no_rank <- is.null(rank) || identical(rank, "")

    rank <- paste0(rank, ": ")

    if (no_rank) {

      rank <- ""

    }

    rank <- sprintf(padl, rank)

    xi <- x[[i]]

    for (j in seq_along(xi)) {

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

  sl <- getOption("width") / ncol(df)

  sl <- max(9L, sl)

  for (col in names(df)) {

    if (is.character(df[[col]])) {

      n <- nchar(col)

      n <- max(sl, n)

      df[[col]] <- truncate_string(df[[col]], n)

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

  if (nrows > 0L) {

    obj <- list(df = df, colname_widths = colname_widths)

    df <- format_cols(obj)

    widths <- apply(df, 2L, nchar)

    dim(widths) <- dim(df)

    na_widths <- is.na(widths)

    widths[na_widths] <- 4L

    widths <- apply(widths, 2L, max, na.rm = TRUE)

    widths <- pmax(colname_widths, widths)

  }

  dsply_nc <- 0L

  cumulative_width <- 1L

  if (dsply_nr > 9L) {

    cumulative_width <-  2L

  }

  for (width in widths) {

    i <- dsply_nc + 1L

    cumulative_width <- cumulative_width + widths[[i]] + 1L

    if (cumulative_width > dwdth) {

      break

    }

    dsply_nc <- i

  }

  dsply_cols <- seq_len(dsply_nc)

  print.data.frame(df[, dsply_cols])

  extras_obj <- list(
    x,
    rows = nrows - dsply_nr,
    cols = ncols - dsply_nc,
    dc = dsply_cols
  )

  print_extras(extras_obj)

  invisible(x)

}

#' @noRd

records_msg <- function(obj) {

  x <- obj[[1L]]

  l <- list(nrec_dnld = obj[["dnld"]], nrec_avl = obj[["avl"]])

  nms <- names(l)

  for (i in seq_along(l)) {

    x_nm <- attr(x, nms[[i]])

    if (length(x_nm) > 0L) {

      suf <- paste0("Records ", l[[i]], ": ")

      x_nm <- paste(x_nm, collapse = " + ")

      n <- obj[["width"]] - nchar(suf) - 2L

      x_nm <- truncate_string(x_nm, n)

      cat(suf, x_nm, "\n", sep = "")

    }

  }

}

#' @noRd

format_cols <- function(obj) {

  df <- obj[["df"]]

  colname_widths <- obj[["colname_widths"]]

  for (i in seq_along(df)) {

    dfi <- df[[i]]

    class <- col_class(dfi)

    if (length(class) > 0L) {

      if (is.list(dfi)) {

        dfi_len <- lapply(dfi, length)

        dfi_na <- lapply(dfi, is.na)

        dfi_na <- lapply(dfi_na, sum)

        dfi <-  unlist(dfi_len) - unlist(dfi_na)

        suffix <- ifelse(dfi == 1L, "", "s")

        dfi <- paste0(dfi, " element", suffix)

      } else {

        if (identical(class, "uri")) {

          dfi <- truncate_string_to_unique(dfi)

        } else if (identical(class, "character")) {

          dfi <- truncate_string(dfi)

        } else {

          cwi <- colname_widths[[i]]

          dfi <- formatC(dfi,  cwi - 2L, cwi, flag = "- ")

        }

      }

    }

    df[[i]] <- dfi

  }

  df

}

#' @noRd

col_class <- function(x) {

  uris <- grepl("^http", x)

  if (any(uris)) {

    "uri"

  } else if (is.integer(x)) {

    "integer"

  } else if (is.numeric(x)) {

    "double"

  } else {

    "character"

  }

}

#' @noRd

print_extras <- function(obj) {

  extra_cols <- obj[["cols"]]

  extra_rows <- obj[["rows"]]

  no_cols <- extra_cols < 1L

  if (no_cols && extra_rows < 1L) {

    return(NULL)

  }

  suffix <- ""

  if (extra_rows > 1L) {

    suffix <- "s"

  }

  cat("...with ", extra_rows, " more record", suffix, sep = "")

  if (no_cols) {

    cat("\n")

    return(NULL)

  }

  suffix <- ""

  if (extra_cols > 1L) {

    suffix <- "s"

  }

  cat(" and ", extra_cols, " more variable", suffix, ":\n", sep = "")


  extra_names <- names(obj[[1L]])

  dsply_cols <- obj[["dc"]]

  extra_names <- extra_names[-dsply_cols]

  extra_name_i <- extra_names[[1L]]

  cat(extra_name_i)

  nchars <- nchar(extra_name_i) + 2L

  sq <- seq_along(extra_names)

  width <- getOption("width")

  for (i in sq[-1L]) {

    extra_name_i <- extra_names[[i]]

    nchars_next <- nchar(extra_name_i) + 2L

    nchars <- nchars + nchars_next

    sep <- ", "

    if (nchars > width) {

      sep <- ",\n"

      nchars <- nchars_next

    }

    cat(sep, extra_name_i, sep = "")

  }

  cat("\n")

}
