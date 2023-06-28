# misc -------------------------------------------------------------------------

#' @noRd

get_next_lowest_factor <- function(
  x,
  y
) {

  x <- as.integer(x)

  y <- as.integer(y)

  if (identical(x %% y, 0L)) {

    return(y)

  }

  y <- y - 1L

  get_next_lowest_factor(x, y)

}

#' @noRd

get_el_recurse <- function(
  obj,
  nms,
  type
) {

  if (length(nms) < 1L) {

    if (is.null(obj) || identical(obj, "")) {

      obj <- cast_to_type(NA, type)

    }

    return(obj)

  }

  nm <- nms[[1L]]

  obj_names <- names(obj)

  has_name <- FALSE

  if (is.null(obj_names)) {

    for (i in obj) {

      has_name <- nm %in% names(i)

      if (has_name) {

        break

      }

    }

  }

  next_obj <- getElement(obj, nm)

  if (has_name) {

    next_obj <- lapply(obj, getElement, nm)

    null_elements <- vapply(next_obj, is.null, NA)

    next_obj[null_elements] <- cast_to_type(NA, type)

    next_obj <- unlist(next_obj, recursive = FALSE)

  }

  get_el_recurse(next_obj, nms[-1L], type)

}

#' @noRd

pb_head <- function(
  msg,
  quiet = FALSE
) {

  nchars <- nchar(msg) + 15L

  diff <- getOption("width") - nchars

  diff <- max(0L, diff)

  body <- rep("=", diff)

  if (!quiet) {

    message("  |=== ", msg, " ", body, "|")

  }

  quiet

}

#' @noRd

truncate_string <- function(
  x,
  sl = 20L
) {

  x <- as.character(x)

  x_sl <- substr(x, 1L, sl - 1L)

  x_sl <- sprintf("%s\u2026", x_sl)

  too_many_chars <- nchar(x) > sl

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

    unique_y <- unique(y)

    cond <- length(unique_y) > 1L && char_all_equal(y, i)

  }

  y_trimmed <- trimws(y)

  unchanged <- y == y_trimmed

  y_trimmed <- paste0("\u2026", y_trimmed)

  y <- ifelse(unchanged, y, y_trimmed)

  x[ind] <- y

  x

}

#' @noRd

char_all_equal <- function(
  x,
  i
) {

  chars <- substr(x, i, i)

  all(chars == chars[[1L]])

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

has_pkgs <- function(...) {

  pkgs <- list(...)

  ans <- vapply(pkgs, requireNamespace, NA, quietly = TRUE)

  all(ans)

}

#' @noRd

name_chr_vec <- function(
  x = NULL,
  unique = TRUE,
  na_rm = TRUE
) {

  if (!is.null(x)) {

    stopifnot("'x' is not a character vector" = inherits(x, "character"))

    if (na_rm) {

      x <- x[!is.na(x)]

    }

    nms <- names(x)

    if (is.null(nms)) {

      nms <- x

    } else {

      nms <- ifelse(nms == "", x, nms)

    }

    if (unique) {

      nms <- make.unique(nms)

    }

    names(x) <- nms

  }

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

  if (any(not_na)) {

    ans <- paste(x[not_na], collapse = "; ")

  }

  ans

}

#' @noRd

cast_to_type <- function(
  x,
  type
) {

  switch(
    type,
    double = as.double(x),
    integer = as.integer(x),
    logical = as.logical(x),
    as.character(x)
  )

}

#' @noRd

all_na <- function(x) {

  na <- is.na(x)

  all(na)

}

#' @noRd

get_rows <- function(
  rows,
  df
) {

  df[rows, , drop = FALSE]

}

#' @noRd

cache_is_valid <- function(timeout, created) {

  timeout_offset <- getOption("finbif_timeout_offset")

  timeout_offset <- pmax(timeout_offset, 0)

  timeout_offset <- pmin(timeout_offset, 1)

  timeout_offset <- timeout_offset * 1000

  timeout_seq <- seq(1000 - timeout_offset, 1000 + timeout_offset)

  seq_length <- length(timeout_seq)

  timeout_secs <- timeout * timeout_seq[sample.int(seq_length, 1L)] * 3.6

  timeout_secs > difftime(Sys.time(), created, units = "secs")

}

#' @noRd

infer_cache <- function(cache) {

  ans <- getOption("finbif_use_cache_metadata")

  if (is.logical(cache)) {

    if (is.logical(ans)) {

      ans <- cache || ans

    }

  } else {

    if (is.logical(ans)) {

      ans <- cache

    }

  }

  ans

}

# random sampling --------------------------------------------------------------

#' @noRd

sample_with_seed <- function(
  n,
  size,
  seed
) {

  on.exit({

    rm(".Random.seed", pos = 1L)

  })

  if (exists(".Random.seed", 1L)) {

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

#' @importFrom digest digest
#' @noRd

gen_seed <- function(x) {

  hash <- lapply(x, getElement, "hash")

  hash <- digest::digest(hash)

  hash <- substr(hash, 1L, 7L)

  strtoi(hash, 16L)

}

# errors -----------------------------------------------------------------------
# modified from https://github.com/reside-ic/defer/blob/master/R/defer.R

#' @noRd

deferrable_error <- function(message) {

  withRestarts(
    {
      calls <- sys.calls()

      i <- length(calls) - 1L

      i <- max(i, 1L)

      e <- error(message, "deferrable_error", call = calls[[i]], calls = calls)

      stop(e)

    },
    continue_deferrable_error = continue
  )

}

#' @noRd

continue <- function(...) {

  NULL

}

#' @noRd

defer_errors <- function(
  expr,
  handler = stop
) {

  errors <- list()

  calls <- sys.calls()

  value <- withCallingHandlers(
    expr,
    deferrable_error = function(e) {

      sq <- seq_along(calls)

      e_calls <- e[["calls"]]

      if (identical(calls[], e_calls[sq])) {

        l <- length(calls) + 1L

        e_calls <- e_calls[-seq_len(l)]

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

deferred_errors <- function(
  errors,
  handler,
  calls,
  value = NULL
) {

  if (length(errors) > 0L) {

    err <- list(errors = errors, value = value)

    class(err) <- c("dfrd_errors", "error", "condition")

    handler(err)

  } else {

    value

  }

}

#' @noRd

error <- function(
  message,
  class,
  ...
) {

  message <- list(message = message, ...)

  structure(message, class = c(class, "error", "condition"))

}

#' @export
#' @noRd

conditionMessage.dfrd_errors <- function(c) {

  errors <- vapply(c[["errors"]], getElement, "", "message")

  n <- length(errors)

  n_errors <- ngettext(n, "error", "errors")

  errors <- paste0("  - ", errors, collapse = "\n")

  sprintf("%d %s occurred:\n%s", n, n_errors, errors)

}

# localization -----------------------------------------------------------------

#' @noRd

get_locale <- function() {

  supported_langs <- sysdata("supported_langs")

  ans <- supported_langs[[1L]]

  env <- c("LANGUAGE", "LANG")

  env <- Sys.getenv(env)

  collate <- Sys.getlocale("LC_COLLATE")

  for (l in c(env, collate)) {

    reg <- regexpr(".+?(?=[[:punct:]])", l, perl = TRUE)

    l <- regmatches(l, reg)

    if (length(l) > 0L) {

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

with_locale <- function(
  x,
  locale = getOption("finbif_locale")
) {

  l <- length(x)

  ans <- NA_character_

  if (identical(l, 1L)) {

    ans <- x[[1L]]

  } else if (l > 1L) {

    nms <- names(x)

    supported_langs <- sysdata("supported_langs")

    locales <- setdiff(supported_langs, locale)

    locales <- c(locale, locales)

    ind <- intersect(locales, nms)

    ind <- ind[[1L]]

    ans <- x[[ind]]

  }

  ans

}
