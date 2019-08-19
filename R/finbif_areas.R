#' Countries, provinces, ,unicipalities and other locations
#'
#' Display country, province, etc., names and codes used in the FinBIF database.
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
    country, row.names = seq_len(nrow(country)),
    names = c("english_name", "finnish_name", "alpha_2", "alpha_3")
  )
}

#' @rdname finbif_countries
#' @export
finbif_provinces <- function() {
  structure(
    province, row.names = seq_len(nrow(province)),
    names = c("english_name", "finnish_name", "alpha", "country")
  )
}

#' @rdname finbif_countries
#' @export
finbif_municipalities <- function() {
  structure(
    municipality, row.names = seq_len(nrow(municipality)),
    names = c("english_name", "finnish_name", "country")
  )
}

#' @rdname finbif_countries
#' @export
finbif_bird_assoc_areas <- function() {
  structure(
    bird_assoc_area, row.names = seq_len(nrow(bird_assoc_area)),
    names = c("name", "code")
  )
}

