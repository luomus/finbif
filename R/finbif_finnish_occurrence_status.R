#' Taxon Finnish occurrence statuses
#'
#' Display the description and codes for Finnish occurrence status used in the
#' FinBIF database.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display countries, provinces or municipalities
#' finbif_finnish_occurrence()
#' }
#' @export
finbif_finnish_occurrence_status <- function() {
  structure(
    finnish_occurrence_status,
    row.names = seq_len(nrow(finnish_occurrence_status)),
    names = c("description", "code")
  )
}
