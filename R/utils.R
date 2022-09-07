# misc -------------------------------------------------------------------------

#' @noRd
#' @description Drop columns from a data.frame where all elements are missing.
#' @param df A `finbif_occ` object.
#' @param which Logical. A vector indicating which columns to check for missing
#'   data. Values recycled to length of `df`. Defaults to all columns.
drop_na_col <- function(df, which = TRUE) {

  which <- rep_len(which, length(df))

  is_na <- lapply(df, is.na)
  is_na <- vapply(is_na, all, logical(1L))

  cond <- !is_na | !which

  attr(df, "column_names") <- attr(df, "column_names")[cond]

  df[, cond, drop = FALSE]

}

#' @noRd
to_sentence_case <- function(string) {
  paste0(substring(toupper(string), 1L, 1L), substring(tolower(string), 2L))
}

#' @noRd
get_next_lowest_factor <- function(x, y) {
  if (x %% y) get_next_lowest_factor(x, y - 1L) else y
}

#' @noRd
#' @importFrom methods as
#' @importFrom utils hasName
get_el_recurse <- function(obj, nms, type) {

  type_na <- methods::as(NA, type)

  if (length(nms) < 1L) {
    return(
      if (is.null(obj) || identical(obj, "")) type_na else obj
    )
  }

  nm <- nms[[1L]]

  if (is.null(names(obj)) && any(vapply(obj, utils::hasName, NA, nm))) {

    obj <- lapply(obj, getElement, nm)

    null_elements <- vapply(obj, is.null, NA)

    obj[null_elements] <- type_na

    obj <- unlist(obj, recursive = FALSE)

  } else {

    obj <- getElement(obj, nm)

  }

  get_el_recurse(obj, nms[-1L], type)

}

#' @noRd
pb_head <- function(msg, quiet = FALSE) {
  gap <- nchar(msg) + 15L
  if (!quiet) {
    message(
      "  |=== ", msg, " ", rep("=", max(0L, getOption("width") - gap)), "|"
    )
  }
}

#' @noRd
truncate_string <- function(x, sl = 20L) {
  x <- as.character(x)
  ifelse(nchar(x) > sl, sprintf("%s\u2026", substr(x, 1L, sl - 1L)), x)
}

#' @noRd
truncate_string_to_unique <- function(x) {
  ind <- !is.na(x)
  y <- x[ind]
  i <- 0L
  all_equal <- TRUE
  while (all_equal && length(unique(y)) > 1L) {
    substr(y, i, i) <- " "
    i <- i + 1L
    j <- substr(y, i, i)
    all_equal <- all(j == j[[1L]])
  }
  y <- trimws(y)
  x[ind] <- ifelse(x[ind] == y, y, paste0("\u2026", y))
  x
}

#' @noRd
value <- function(obj) obj

#' @noRd
col_type_string <- function(dwc) {
  if (dwc) {
    "dwc"
  } else {
    "translated_var"
  }
}

#' @noRd
det_datetime_method <- function(method, n) {

  if (missing(method)) {

    method <- "none"

    n <- sum(ifelse(is.numeric(n) & n >= 0L, n, Inf))

    if (n < 1e5) {

      method <- "fast"

    }

  }

  method

}

#' @noRd
#' @importFrom tools file_ext
open_tsv_connection <- function(file, tsv, mode = "rt") {

  if (!identical(tools::file_ext(file), "tsv")) {

    con <- unz(file, tsv, mode)

  } else {

    con <- file(file, mode)

  }

  con

}

#' @noRd
nlines <- function(file, tsv) {

  on.exit(close(con))

  con <- open_tsv_connection(file, tsv, "rb")

  n <- 0L

  cond <- TRUE

  while (cond) {

    chunk <- readBin(con, "raw", 65536L)

    n <- n + sum(chunk == as.raw(10L))

    cond <- !identical(chunk, raw(0L))

  }

  n - 1L
}

#' @noRd
has_pkgs <- function(...) {
  pkgs <- list(...)
  ans <- vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)
  all(ans)
}

#' @noRd
name_chr_vec <- function(x, unique = TRUE, na.rm = TRUE) { # nolint

  stopifnot(inherits(x, "character"))

  if (na.rm) {

    x <- x[!is.na(x)]

  }

  nms <- names(x)

  if (is.null(nms)) {

    names(x) <- x

  } else {

    names(x) <- ifelse(nms == "", x, nms)

  }

  if (unique) {

    names(x) <- make.unique(names(x))

  }

  x

}

#' @noRd
remove_domain <- function(x, domain = "tun.fi", protocol =  "http") {

  sub(sprintf("^%s://%s/", protocol, domain), "", x)

}

# random sampling --------------------------------------------------------------

#' @noRd
sample_with_seed <- function(n, size, seed) {
  if (exists(".Random.seed", 1L)) {
    oldseed <- get(".Random.seed", 1L)
    on.exit(assign(".Random.seed", oldseed, 1L))
  } else {
    on.exit(rm(".Random.seed", pos = 1L))
  }
  args <- list(seed, "default", "default")
  if (getRversion() >= "3.6.0") args <- c(args, "default")
  do.call(set.seed, args)
  sample.int(n, size)
}

#' @noRd
gen_seed <- function(x, ...) UseMethod("gen_seed", x)

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
      call <- calls[[max(length(calls) - 1L, 1L)]]
      stop(error(message, "deferrable_error", call = call, calls = calls))
    },
    continue_deferrable_error = function(...) NULL
  )
}

#' @noRd
defer_errors <- function(expr, handler = stop) {
  errors <- list()

  calls <- sys.calls()
  value <- withCallingHandlers(
    expr,
    deferrable_error = function(e) {
      if (identical(calls[], e[["calls"]][seq_along(calls)])) {
        e[["calls"]] <- e[["calls"]][-seq_len(length(calls) + 1L)]
      }
      errors <<- c(errors, list(e))
      invokeRestart("continue_deferrable_error")
    }
  )

  deferred_errors(errors, handler, calls, value)
}

#' @noRd
deferred_errors <- function(errors, handler, calls, value = NULL) {
  if (length(errors)) {
    err <- list(errors = errors, value = value)
    class(err) <- c("dfrd_errors", "error", "condition")
    handler(err)
  } else {
    value
  }
}

#' @noRd
error <- function(message, class, ...) {
  structure(
    list(message = message, ...), class = c(class, "error", "condition")
  )
}

#' @export
#' @noRd
conditionMessage.dfrd_errors <- function(c) {
  errors <- vapply(c[["errors"]], "[[", character(1), "message")
  n <- length(errors)
  sprintf(
    "%d %s occurred:\n%s", n, ngettext(n, "error", "errors"),
    paste0("  - ", errors, collapse = "\n")
  )
}

# variable names ---------------------------------------------------------------

#' @noRd
to_ <- function(x, from, to) {
  x      <- unlist(x)
  ind    <- !x %in% c(var_names[[to]], "default_vars")
  x[ind] <- var_names[match(x[ind], var_names[[from]]), to]
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
to_dwc <- function(...) to_(list(...), "translated_var", "dwc")

#' @rdname to_dwc
#' @export
to_native <- function(...) to_(list(...), "dwc", "translated_var")

#' @rdname to_dwc
#' @export
from_schema <- function(
  ..., to = c("native", "dwc", "short"), file = c("none", "citable", "lite")
) {

  nms <- make.names(c(...))

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
