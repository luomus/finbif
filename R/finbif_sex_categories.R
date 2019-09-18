#' FinBIF sex categories
#'
#' Display organism sex categories used in the FinBIF database.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display list of sex categories and codes
#' finbif_sex_categories()
#' }
#' @export
finbif_sex_categories <- function() {
  sex <- sex[order(sex[["category"]]), ]
  structure(sex, row.names = seq_len(nrow(sex)))
}

