#' @noRd
#' @importFrom lubridate as_date period rollback
parse_date <- function(x) {

  if (is.null(x)) return(x)
  if (grepl("^\\d{4}$", x)) return(structure(x, class = "y"))
  if (grepl("^\\d{4}-\\d{2}$", x)) return(structure(x, class = "ym"))

  tryCatch(
    lubridate::as_date(x),
    warning = function(w) {
      deferrable_error("Can not parse one or more specified dates")
    }
  )

}

#' @noRd
dates <- function(filter, begin = NULL, end = NULL) {
  if (!inherits(begin, "Interval") && !any(nchar(c(begin, end)))) {
    deferrable_error("Need to specify at least one of 'begin' or 'end' date")
  }
  switch(
    filter,
    date_range_ymd = date_range_ymd(begin, end),
    date_range_ym  = date_range_ym(begin, end),
    date_range_d   = date_range_d(begin, end),
    date_range_md  = date_range_md(begin, end),
    last_import_date_min = paste(parse_date(begin)),
    last_import_date_max =  paste(parse_date(begin)),
    first_import_date_min =  paste(parse_date(begin)),
    first_import_date_max =  paste(parse_date(begin))
  )
}

#' @noRd
date_range_ymd <- function(x, y, format = "%Y-%m-%d") {

  if (inherits(x, "Interval")) {
    y <- lubridate::int_end(x)
    x <- lubridate::int_start(x)
    return(date_range_ymd(x, y, format))
  }

  x <- parse_date(x)
  y <- parse_date(y)

  if (is.null(x) || is.null(y) || inherits(x, class(y))) {
    if (inherits(x, "Date")) x <- format.Date(x, format)
    if (inherits(y, "Date")) y <- format.Date(y, format)
    return(paste(c(x, y), collapse = "/"))
  }

  date_range_ymd2(x, y, format)

}

#' @noRd
date_range_ymd2 <- function(x, y, format) {
  if (inherits(x, "y")) {
    x <- paste0(x, "-01")
    return(date_range_ymd(x, y, format))
  }

  if (inherits(y, "y")) {
    y <- paste0(y, "-12")
    return(date_range_ymd(x, y, format))
  }

  if (inherits(x, "ym")) {
    x <- paste0(x, "-01")
    return(date_range_ymd(x, y, format))
  }

  if (inherits(y, "ym")) {
    y <- paste0(y, "-01")
    y <- lubridate::rollback(
      lubridate::as_date(y) + lubridate::period(month = 1L)
    )
    return(date_range_ymd(x, y, format))
  }

}

#' @noRd
date_range_ym  <- function(x, y) date_range_ymd(x, y, "%Y-%m")

#' @noRd
date_range_d <- function(x, y) paste(x, y, sep = "/")

#' @noRd
date_range_md <- function(x, y) {
  x <- as.integer(sub("-", "", x))
  y <- as.integer(sub("-", "", y))
  paste(x, y, sep = "/")
}
