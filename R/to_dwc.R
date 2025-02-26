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
  to = c("native", "dwc"),
  file = c("none", "citable", "lite")
) {

  nms <- c(...)

  nms <- make.names(nms)

  vars <- switch(
    match.arg(file),
    none = sysdata(list(which = "var_names")),
    citable = sysdata(list(which = "cite_file_vars")),
    lite = sysdata(list(which = "lite_download_file_vars"))
  )

  to <- switch(
    match.arg(to),
    native = "translated_var",
    dwc = "dwc"
  )

  vars[nms, to]

}

#' @noRd

to <- function(
  x,
  from,
  to
) {

  x <- unlist(x)

  var_names <- sysdata(list(which = "var_names"))

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
