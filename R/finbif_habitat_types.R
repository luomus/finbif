#' FinBIF habitat types
#'
#' Display the habitat types and specific habitat types used in the FinBIF
#' database.
#'
#' @return A data.frame.
#' @examples
#'
#' # Display the administrative statuses used by FinBIF
#' finbif_habitat_types()
#' finbif_habitat_qualifiers()
#'
#' @export
finbif_habitat_types <- function() {
  df <- habitat_types[["habitat_types"]]
  structure(df, row.names = seq_len(nrow(df)))
}

#' @rdname finbif_habitat_types
#' @export
finbif_habitat_qualifiers <- function() {
  df <- habitat_types[["specific_habitat_types"]]
  structure(df, row.names = seq_len(nrow(df)))
}
