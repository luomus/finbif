#' @noRd

dates <- function(obj) {

  len <- length(obj)

  if (len < 2L) {

    deferrable_error("Need to specify at least one of 'begin' or 'end' date")

  }

  if (is.null(obj[["end"]])) {

    obj[["begin"]] <- obj[[2L]]

    if (len > 2L) {

      obj[["end"]] <- obj[[3L]]

    }

  }

  obj[["format"]] <- "%Y-%m-%d"

  ans <- switch(
    obj[["filter"]],
    date_range_ymd = date_range_ymd(obj),
    date_range_ym  = date_range_ym(obj),
    date_range_d = date_range_md(obj),
    date_range_md = date_range_md(obj),
    parse_date(obj[["begin"]])
  )

  paste(ans)

}

#' @noRd
#' @importFrom lubridate int_end int_start period rollback

date_range_ymd <- function(obj) {

  if (inherits(obj[["begin"]], "Interval")) {

    obj[["end"]] <- lubridate::int_end(obj[["begin"]])

    obj[["begin"]] <- lubridate::int_start(obj[["begin"]])

  } else {

    obj[["begin"]] <- parse_date(obj[["begin"]])

    obj[["end"]] <- parse_date(obj[["end"]])

    no_begin <- identical(obj[["begin"]], "")

    no_end <- identical(obj[["end"]], "")

    null_begin <- is.null(obj[["begin"]])

    null_end <- is.null(obj[["end"]])

    end_class <- class(obj[["end"]])

    same_class <- inherits(obj[["begin"]], end_class)

    cond <- any(no_begin, no_end, null_begin, null_end, same_class)

    if (cond) {

      return(format_date(obj))

    }

    if (inherits(obj[["begin"]], "y")) {

      obj[["begin"]] <- paste0(obj[["begin"]], "-01")

    } else if (inherits(obj[["end"]], "y")) {

      obj[["end"]] <- paste0(obj[["end"]], "-12")

    } else if (inherits(obj[["begin"]], "ym")) {

      obj[["begin"]] <- paste0(obj[["begin"]], "-01")

    } else if (inherits(obj[["end"]], "ym")) {

      obj[["end"]] <- paste0(obj[["end"]], "-01")

      obj[["end"]] <- as.Date(obj[["end"]]) + lubridate::period(month = 1L)

      obj[["end"]] <- lubridate::rollback(obj[["end"]])

    }

  }

  date_range_ymd(obj)

}

#' @noRd

parse_date <- function(date) {

  if (!is.null(date) && as.character(date) != "") {

    if (grepl("^\\d{4}$", date)) {

      date <- structure(date, class = "y")

    } else if (grepl("^\\d{4}-\\d{2}$", date)) {

      date <- structure(date, class = "ym")

    } else {

      date <- tryCatch(
        as.Date(date),
        warning = function(w) {

          deferrable_error("Can not parse one or more specified dates")

        }
      )

    }

  }

  date

}

#' @noRd

format_date <- function(obj) {

  if (inherits(obj[["begin"]], "Date")) {

    obj[["begin"]] <- format.Date(obj[["begin"]], obj[["format"]])

  }

  if (inherits(obj[["end"]], "Date")) {

    obj[["end"]] <- format.Date(obj[["end"]], obj[["format"]])

  }

  dates <- c(obj[["begin"]], obj[["end"]])

  paste(dates, collapse = "/")

}

#' @noRd

date_range_ym  <- function(obj) {

  obj[["format"]] <- "%Y-%m"

  date_range_ymd(obj)

}

#' @noRd

date_range_md <- function(obj) {

  dates <- c(obj[["begin"]], obj[["end"]])

  dates <- sub("-", "", dates)

  dates <- as.integer(dates)

  paste(dates, collapse = "/")

}
