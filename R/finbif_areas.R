#' Countries, Provinces, and Municipalities
#'
#' Display country, province or municipality names and codes used in the FinBIF
#' database.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display countries, provinces or municipalities
#' finbif_countries()
#' finbif_provinces()
#' finbif_municipalities()
#' }
#' @export
finbif_countries <- function() {
  structure(
    countries, row.names = seq_len(nrow(countries)),
    names = c("english_name", "finnish_name", "alpha_2", "alpha_3")
  )
}

#' @rdname finbif_countries
#' @export
finbif_provinces <- function() {
  structure(
    provinces, row.names = seq_len(nrow(provinces)),
    names = c("english_name", "finnish_name", "alpha", "country")
  )
}

#' @rdname finbif_countries
#' @export
finbif_municipalities <- function() {
  structure(
    countries, row.names = seq_len(nrow(countries)),
    names = c("english_name", "finnish_name", "country")
  )
}
