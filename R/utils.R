#' @importFrom digest digest

# misc -------------------------------------------------------------------------

#' @noRd
to_sentence_case <- function(string)
  paste0(substring(toupper(string), 1L, 1L), substring(tolower(string), 2L))

#' @noRd
get_next_lowest_factor <-
  function(x, y) if (x %% y) get_next_lowest_factor(x, y - 1L) else y

#' @noRd
#' @importFrom methods as
get_el_recurse <- function(obj, nms, type) {
  if (length(nms) < 1L) return(if (is.null(obj)) methods::as(NA, type) else obj)
  obj <- getElement(obj, nms[[1L]])
  get_el_recurse(obj, nms[-1L], type)
}

#' @noRd
pb_head <- function(msg) {
  gap <- nchar(msg) + 15L
  message("  |=== ", msg, " ", rep("=", max(0L, getOption("width") - gap)), "|")
}

#' @noRd
truncate_string <- function(x, sl = 20L) {
  x <- as.character(x)
  ifelse(nchar(x) > sl, sprintf("%s\u2026", substr(x, 1L, sl - 1L)), x)
}

#' @noRd
truncate_string_to_unique <- function(x) {
  i <- 0L
  all_equal <- TRUE
  while (all_equal) {
    substr(x, i, i) <- " "
    i <- i + 1L
    j <- substr(x, i, i)
    all_equal <- all(j == j[[1L]])
  }
  trimws(x)
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
deferrable_error <- function(message)
  withRestarts({
    calls <- sys.calls()
    call <- calls[[max(length(calls) - 1L, 1L)]]
    stop(error(message, "deferrable_error", call = call, calls = calls))
  },
  continue_deferrable_error = function(...) NULL)

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
deferred_errors <- function(errors, handler, calls, value = NULL)
  if (length(errors)) {
    err <- list(errors = errors, value = value)
    class(err) <- c("dfrd_errors", "error", "condition")
    handler(err)
  } else {
    value
  }

#' @noRd
error <- function(message, class, ...)
  structure(
    list(message = message, ...), class = c(class, "error", "condition")
  )

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

#' Convert variable names to and from Darwin Core style
#'
#' Convert FinBIF native variable names to Darwin Core style variable names or
#' vice versa.
#'
#' @param ... Character. Variable names in FinBIF native or Darwin Core
#'   style.
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

# localization -----------------------------------------------------------------
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
      if (supported_langs[[l]] %in% supported_langs) {
        ans <- supported_langs[[l]]
        break
      }
    }
  }

  ans

}
