#' FinBIF record basis types
#'
#' Display information on the basis of FinBIF records.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display record basis metadata
#' finbif_record_basis()
#' }
#' @export
finbif_record_basis <- function() {
  structure(
    record_basis[c("name", "description")],
    row.names = seq_len(nrow(record_basis))
  )
}
