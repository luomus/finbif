#' @noRd

dates <- function(obj) {

  if (is.null(obj[["end"]])) {

    obj[["begin"]] <- obj[[2L]]

    if (length(obj) > 2L) {

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

date_range_ymd <- function(obj) {

  if (inherits(obj[["begin"]], "Interval")) {

    begin <- getElement(obj[["begin"]], "start")

    obj[["end"]] <- begin + getElement(obj[["begin"]], ".Data")

    obj[["begin"]] <- begin

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

      obj[["end"]] <- as.Date(obj[["end"]])

      obj[["end"]] <- seq(obj[["end"]], by = "month", length.out = 2L)[[2L]]

      obj[["end"]] <- seq(obj[["end"]], by = "-1 day", length.out = 2L)[[2L]]

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

      date <- tryCatch(as.Date(date), error = date_error)

    }

  }

  date

}

#' @noRd

date_error <- function(e) {

  deferrable_error("Can't parse one or more specified dates")

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
