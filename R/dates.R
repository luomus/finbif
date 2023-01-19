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
#' @importFrom lubridate int_end int_start

date_range_ymd <- function(obj) {

  begin <- obj[["begin"]]

  end <- obj[["end"]]

  is_interval <- inherits(begin, "Interval")

  if (is_interval) {

    obj[["begin"]] <- lubridate::int_start(begin)

    obj[["end"]] <- lubridate::int_end(begin)

    ans <- date_range_ymd(obj)

    return(ans)

  }

  begin <- parse_date(begin)

  obj[["begin"]] <- begin

  end <- parse_date(end)

  obj[["end"]] <- end

  no_begin <- identical(begin, "")

  no_end <- identical(end, "")

  null_begin <- is.null(begin)

  null_end <- is.null(end)

  end_class <- class(end)

  same_class <- inherits(begin, end_class)

  cond <- any(no_begin, no_end, null_begin, null_end, same_class)

  if (cond) {

    ans <- format_date(obj)

    return(ans)

  }

  date_range_ymd2(obj)

}

#' @noRd
#' @importFrom lubridate as_date

parse_date <- function(date) {

  date_null <- is.null(date)

  date_chr <- as.character(date)

  date_empty <- identical(date_chr, "")

  no_date <- date_null || date_empty

  if (no_date) {

    return(date)

  }

  is_year <- grepl("^\\d{4}$", date)

  if (is_year) {

    ans <- structure(date, class = "y")

    return(ans)

  }

  is_year_month <- grepl("^\\d{4}-\\d{2}$", date)

  if (is_year_month) {

    ans <- structure(date, class = "ym")

    return(ans)

  }

  tryCatch({

     lubridate::as_date(date)

    },
    warning = function(w) {

      deferrable_error("Can not parse one or more specified dates")

    }
  )

}

#' @noRd

format_date <- function(obj) {

  begin <- obj[["begin"]]

  begin_is_date <- inherits(begin, "Date")

  end <- obj[["end"]]

  end_is_date <- inherits(end, "Date")

  format <- obj[["format"]]

  if (begin_is_date) {

    begin <- format.Date(begin, format)

  }

  if (end_is_date) {

    end <- format.Date(end, format)

  }

  dates <- c(begin, end)

  paste(dates, collapse = "/")

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
