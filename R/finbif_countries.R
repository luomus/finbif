#' Countries
#'
#' Display the country names and codes used in the FinBIF database.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display the country names and codes used in FinBIF
#' finbif_countries()
#' }
#' @export
finbif_countries <- function() {
  structure(
    countries, row.names = seq_len(nrow(countries)),
    names = c("finnish_name", "english_name", "alpha_2", "alpah_3")
  )
}
