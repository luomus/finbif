# misc -------------------------------------------------------------------------

#' @noRd

drop_na_col <- function(df) {

  ncols <- length(df)

  which <- attr(df, "drop_na", TRUE)

  column_names <- attr(df, "column_names")

  which <- rep_len(which, ncols)

  is_na <- lapply(df, is.na)

  is_na <- vapply(is_na, all, NA)

  cond <- !is_na | !which

  column_names <- column_names[cond]

  attr(df, "column_names") <- column_names

  df[, cond, drop = FALSE]

}

#' @noRd

to_sentence_case <- function(string) {

  upper <- toupper(string)

  first <- substring(upper, 1L, 1L)

  lower <- tolower(string)

  rest <- substring(lower, 2L)

  paste0(first, rest)

}

#' @noRd

get_next_lowest_factor <- function(x, y) {

  mod <- x %% y

  end <- identical(mod, 0L)

  if (end) {

    return(y)

  }

  y <- y - 1L

  get_next_lowest_factor(x, y)

}

#' @noRd
#' @importFrom utils hasName

get_el_recurse <- function(obj, nms, type) {

  type_na <- cast_to_type(NA, type)

  many_names <- length(nms) < 1L

  if (many_names) {

    is_null <- is.null(obj)

    is_empty <- identical(obj, "")

    to_na <- is_null || is_empty

    if (to_na) {

      obj <- type_na

    }

    return(obj)

  }

  nm <- nms[[1L]]

  obj_nms <- names(obj)

  obj_nms_is_null <- is.null(obj_nms)

  has_nm <- vapply(obj, utils::hasName, NA, nm)

  any_has_nm <- any(has_nm)

  unlist_obj <- obj_nms_is_null && any_has_nm

  if (unlist_obj) {

    obj <- lapply(obj, getElement, nm)

    null_elements <- vapply(obj, is.null, NA)

    obj[null_elements] <- type_na

    obj <- unlist(obj, recursive = FALSE)

  } else {

    obj <- getElement(obj, nm)

  }

  nms <- nms[-1L]

  get_el_recurse(obj, nms, type)

}

#' @noRd

pb_head <- function(msg, quiet = FALSE) {

  nchars <- nchar(msg)

  gap <- nchars + 15L

  width <- getOption("width")

  diff <- width - gap

  diff <- max(0L, diff)

  body <- rep("=", diff)

  if (!quiet) {

    message("  |=== ", msg, " ", body, "|")

  }

}

#' @noRd

truncate_string <- function(x, sl = 20L) {

  x <- as.character(x)

  nchars <- nchar(x)

  too_many_chars <- nchars > sl

  sl <- sl - 1L

  x_sl <- substr(x, 1L, sl)

  x_sl <- sprintf("%s\u2026", x_sl)

  ifelse(too_many_chars, x_sl, x)

}

#' @noRd

truncate_string_to_unique <- function(x) {

  ind <- !is.na(x)

  y <- x[ind]

  i <- 0L

  cond <- TRUE

  while (cond) {

    substr(y, i, i) <- " "

    i <- i + 1L

    j <- substr(y, i, i)

    j1 <- j[[1L]]

    equal <- j == j1

    all_equal <- all(equal)

    unique_y <- unique(y)

    n_unique_y <- length(unique_y)

    more_than_one <- n_unique_y > 1L

    cond <- all_equal && more_than_one

  }

  y <- trimws(y)

  x_ind <- x[ind]

  unchanged <- x_ind == y

  changed <- paste0("\u2026", y)

  x_ind <- ifelse(unchanged, y, changed)

  x[ind] <- x_ind

  x

}

#' @noRd

value <- function(obj) {

  obj

}

#' @noRd

col_type_string <- function(dwc) {

  ans <- "translated_var"

  if (dwc) {

    ans <- "dwc"

  }

  ans

}

#' @noRd

det_datetime_method <- function(method, n) {

  is_null <- is.null(method)

  if (is_null) {

    method <- "none"

    is_num <- is.numeric(n)

    is_pos <- n >= 0L

    cond <- is_num & is_pos

    n <- ifelse(cond, n, Inf)

    n <- sum(n)

    if (n < 1e5) {

      method <- "fast"

    }

  }

  method

}

#' @noRd

open_tsv_connection <- function(file, tsv, mode = "rt") {

  nchars <- nchar(file)

  start <- nchars - 3L

  ext <- substring(file, start, nchars)

  switch(
    ext,
    .tsv = file(file, mode),
    unz(file, tsv, mode)
  )

}

#' @noRd

nlines <- function(file, tsv) {

  con <- open_tsv_connection(file, tsv, "rb")

  on.exit({

    close(con)

  })

  n <- -1L

  cond <- TRUE

  while (cond) {

    chunk <- readBin(con, "raw", 65536L)

    raw10 <- as.raw(10L)

    chunk_10 <- chunk == raw10

    subtotal <- sum(chunk_10)

    n <- n + subtotal

    empty <- raw(0L)

    cond <- !identical(chunk, empty)

  }

  n

}

#' @noRd

has_pkgs <- function(...) {

  pkgs <- list(...)

  ans <- vapply(pkgs, requireNamespace, NA, quietly = TRUE)

  all(ans)

}

#' @noRd

name_chr_vec <- function(x, unique = TRUE, na.rm = TRUE) { # nolint

  no_x <- missing(x)

  if (no_x) {

    return(NULL)

  }

  is_char <- inherits(x, "character")

  stopifnot("'x' is not a character vector" = is_char)

  if (na.rm) {

    not_na <- !is.na(x)

    x <- x[not_na]

  }

  nms <- names(x)

  no_nms <- is.null(nms)

  if (no_nms) {

    nms <- x

  } else {

    empty_names <- nms == ""

    nms <- ifelse(empty_names, x, nms)

  }

  if (unique) {

    nms <- make.unique(nms)

  }

  names(x) <- nms

  x

}

#' @noRd

remove_domain <- function(x) {

  sub("^http://tun.fi/", "", x)

}

#' @noRd

concat_string <- function(x) {

  ans <- NA_character_

  not_na <- !is.na(x)

  any_not_na <- any(not_na)

  if (any_not_na) {

    x_not_na <- x[not_na]

    ans <- paste(x_not_na, collapse = "; ")

  }

  ans

}

#' @noRd

cast_to_type <- function(x, type) {

  l <- length(type)

  cond <- !identical(l, 1L)

  if (cond) {

    type <- "character"

  }

  f <- switch(
    type,
    character = as.character,
    double = as.double,
    integer = as.integer,
    logical = as.logical
  )

  f(x)

}

# random sampling --------------------------------------------------------------

#' @noRd

sample_with_seed <- function(n, size, seed) {

  on.exit({

    rm(".Random.seed", pos = 1L)

  })

  has_seed <- exists(".Random.seed", 1L)

  if (has_seed) {

    oldseed <- get(".Random.seed", 1L)

    on.exit({

      assign(".Random.seed", oldseed, 1L)

    })

  }

  args <- list(seed, "default", "default")

  if (getRversion() >= "3.6.0") {

    args <- c(args, "default")

  }

  do.call(set.seed, args)

  sample.int(n, size)

}

#' @noRd

gen_seed <- function(x, ...) {

  UseMethod("gen_seed", x)

}

#' @importFrom digest digest
#' @export
#' @noRd

gen_seed.finbif_records_list <- function(x, ...) {

  hash <- lapply(x, getElement, "hash")

  hash <- do.call(paste0, hash)

  hash <- digest::digest(hash)

  hash <- substr(hash, 1L, 7L)

  strtoi(hash, 16L)

}

# errors -----------------------------------------------------------------------
# modified from https://github.com/reside-ic/defer/blob/master/R/defer.R

#' @noRd

deferrable_error <- function(message) {

  withRestarts({

      calls <- sys.calls()

      calls_len <- length(calls)

      i <- calls_len - 1L

      i <- max(i, 1L)

      call <- calls[[i]]

      err <- error(message, "deferrable_error", call = call, calls = calls)

      stop(err)

    },
    continue_deferrable_error = continue
  )

}

#' @noRd

continue <- function(...) {

  NULL

}

#' @noRd

defer_errors <- function(expr, handler = stop) {

  errors <- list()

  calls <- sys.calls()

  value <- withCallingHandlers(
    expr,
    deferrable_error = function(e) {

      sq <- seq_along(calls)

      calls_obj <- calls[]

      e_calls <- e[["calls"]]

      e_calls_obj <- e_calls[sq]

      cond <- identical(calls_obj, e_calls_obj)

      if (cond) {

        l <- length(calls)

        l <- l + 1L

        ind <- seq_len(l)

        ind <- ind * -1L

        e_calls <- e_calls[ind]

        e[["calls"]] <- e_calls

      }

      e_list <- list(e)

      errors <<- c(errors, e_list)

      invokeRestart("continue_deferrable_error")

    }
  )

  deferred_errors(errors, handler, calls, value)
}

#' @noRd

deferred_errors <- function(errors, handler, calls, value = NULL) {

  l <- length(errors)

  if (l) {

    err <- list(errors = errors, value = value)

    class <- c("dfrd_errors", "error", "condition")

    class(err) <- class

    handler(err)

  } else {

    value

  }

}

#' @noRd

error <- function(message, class, ...) {

  message <- list(message = message, ...)

  class <- c(class, "error", "condition")

  structure(message, class = class)

}

#' @export
#' @noRd

conditionMessage.dfrd_errors <- function(c) {

  errors <- c[["errors"]]

  errors <- vapply(errors, getElement, "", "message")

  n <- length(errors)

  n_errors <- ngettext(n, "error", "errors")

  errors <- paste0("  - ", errors, collapse = "\n")

  sprintf("%d %s occurred:\n%s", n, n_errors, errors)

}

# variable names ---------------------------------------------------------------

#' @noRd

to <- function(x, from, to) {

  x <- unlist(x)

  vars_to <- var_names[[to]]

  vars_from <- var_names[[from]]

  vars_to <- c(vars_to, "default_vars")

  ind <- !x %in% vars_to

  xx <- x[ind]

  xx <- match(xx, vars_from)

  xx <- var_names[xx, to]

  x[ind] <- xx

  x

}

#' Convert variable names
#'
#' Convert variable names to Darwin Core or FinBIF R package native style.
#'
#' @param ... Character. Variable names to convert. For `to_dwc` and `to_native`
#'   the names must be in the opposite format. For `from_schema` the names must
#'   be from the FinBIF schema (e.g., names returned by https://api.laji.fi) or
#'   a FinBIF download file (citable or lite).
#' @param to Character. Type of variable names to convert to.
#' @param file Character. For variable names that are derived from a FinBIF
#'   download file which type of file.
#'
#' @return Character vector.
#'
#' @examples
#'
#' to_dwc("record_id", "date_time", "scientific_name")
#' @export

to_dwc <- function(...) {

  l <- list(...)

  to(l, "translated_var", "dwc")

}

#' @rdname to_dwc
#' @export

to_native <- function(...) {

  l <- list(...)

  to(l, "dwc", "translated_var")

}

#' @rdname to_dwc
#' @export

from_schema <- function(
  ...,
  to = c("native", "dwc", "short"),
  file = c("none", "citable", "lite")
) {

  nms <- c(...)

  nms <- make.names(nms)

  file <- match.arg(file)

  vars <- switch(
    file,
    none = var_names,
    citable = cite_file_vars,
    lite = lite_download_file_vars
  )

  to <- match.arg(to)

  to <- switch(to, native = "translated_var", dwc = "dwc", short = "shrtnm")

  vars[nms, to]

}

# localization -----------------------------------------------------------------

#' @noRd
get_locale <- function() {
  ans <- supported_langs[[1L]]
  sys_lang <- c(Sys.getenv(c("LANGUAGE", "LANG")), Sys.getlocale("LC_COLLATE"))

  for (l in sys_lang) {
    l <- regmatches(l, regexpr(".+?(?=[[:punct:]])", l, perl = TRUE))
    if (length(l)) {
      if (l %in% supported_langs) {
        ans <- l
        break
      }
      if (l %in% names(supported_langs)) {
        ans <- supported_langs[[l]]
        break
      }
    }
  }

  ans

}

#' @noRd
with_locale <- function(x, locale = getOption("finbif_locale")) {
  if (identical(length(x), 0L)) return(NA_character_)
  if (identical(length(x), 1L)) return(x[[1L]])
  x[[intersect(c(locale, setdiff(supported_langs, locale)), names(x))[[1L]]]]
}
