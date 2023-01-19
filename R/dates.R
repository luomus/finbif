#' @noRd
#' @importFrom lubridate as_date period rollback int_end int_start

dates <- function(obj) {

  obj_len <- length(obj)

  if (obj_len < 2L) {

    deferrable_error("Need to specify at least one of 'begin' or 'end' date")

  }

  end <- obj[["end"]]

  no_end_name <- is.null(end)

  no_end <- identical(obj_len, 2L)

  end_not_named <- no_end_name && !no_end

  if (end_not_named) {

    begin <- obj[[2L]]

    obj[["begin"]] <- begin

    end <- obj[[3L]]

  }

  obj[["end"]] <- end

  no_end <- no_end && no_end_name

  if (no_end) {

    begin <- obj[[2L]]

    obj[["begin"]] <- begin

  }

  obj[["format"]] <- "%Y-%m-%d"

  filter <- obj[["filter"]]

  ans <- switch(
    filter,
    date_range_ymd = date_range_ymd(obj),
    date_range_ym  = date_range_ym(obj),
    date_range_d   = date_range_d(obj),
    date_range_md  = date_range_md(obj),
    last_import_date_min = parse_date(begin),
    last_import_date_max = parse_date(begin),
    first_import_date_min = parse_date(begin),
    first_import_date_max = parse_date(begin)
  )

  paste(ans)

}

#' @noRd
date_range_ymd <- function(obj) {

  if (inherits(obj[["begin"]], "Interval")) {

    obj[["end"]] <- lubridate::int_end(obj[["begin"]])

    obj[["begin"]] <- lubridate::int_start(obj[["begin"]])

    return(date_range_ymd(obj))

  }

  obj[["begin"]]  <- parse_date(obj[["begin"]])

  obj[["end"]] <- parse_date(obj[["end"]])

  cond <- c(
    identical(obj[["begin"]], ""),
    identical(obj[["end"]], ""),
    is.null(obj[["begin"]]),
    is.null(obj[["end"]]),
    inherits(obj[["begin"]], class(obj[["end"]]))
  )

  if (any(cond)) {

    return(format_date(obj))

  }

  date_range_ymd2(obj)

}

#' @noRd
parse_date <- function(x) {

  if (is.null(x) || identical(as.character(x), "")) {

    return(x)

  }

  if (grepl("^\\d{4}$", x)) {

    return(structure(x, class = "y"))

  }

  if (grepl("^\\d{4}-\\d{2}$", x)) {

    return(structure(x, class = "ym"))

  }

  tryCatch(
    lubridate::as_date(x),
    warning = function(w) {
      deferrable_error("Can not parse one or more specified dates")
    }
  )

}

#' @noRd
format_date <- function(obj) {

  if (inherits(obj[["begin"]], "Date")) {

    obj[["begin"]] <- format.Date(obj[["begin"]], obj[["format"]])

  }

  if (inherits(obj[["end"]], "Date")) {

    obj[["end"]] <- format.Date(obj[["end"]], obj[["format"]])

  }

  paste(c(obj[["begin"]], obj[["end"]]), collapse = "/")

}

#' @noRd
date_range_ymd2 <- function(obj) {

  if (inherits(obj[["begin"]], "y")) {

    obj[["begin"]] <- paste0(obj[["begin"]], "-01")

    return(date_range_ymd(obj))

  }

  if (inherits(obj[["end"]], "y")) {

    obj[["end"]] <- paste0(obj[["end"]], "-12")

    return(date_range_ymd(obj))

  }

  if (inherits(obj[["begin"]], "ym")) {

    obj[["begin"]] <- paste0(obj[["begin"]], "-01")

    return(date_range_ymd(obj))

  }

  if (inherits(obj[["end"]], "ym")) {

    obj[["end"]] <- paste0(obj[["end"]], "-01")

    obj[["end"]] <- lubridate::rollback(
      lubridate::as_date(obj[["end"]]) + lubridate::period(month = 1L)
    )

    return(date_range_ymd(obj))

  }

}

#' @noRd
date_range_ym  <- function(obj) {

  obj[["format"]] <- "%Y-%m"

  date_range_ymd(obj)

}

#' @noRd
date_range_d <- function(obj) {

  paste(obj[["begin"]], obj[["end"]], sep = "/")

}

#' @noRd
date_range_md <- function(obj) {

  obj[["begin"]] <- as.integer(sub("-", "", obj[["begin"]]))

  obj[["end"]] <- as.integer(sub("-", "", obj[["end"]]))

  paste(obj[["begin"]], obj[["end"]], sep = "/")

}
