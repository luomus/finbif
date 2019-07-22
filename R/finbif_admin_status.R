#' FinBIF administrative status
#'
#' Display the administrative statuses used in the FinBIF database.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display the administrative statuses used by FinBIF
#' finbif_admin_status()
#' }
#' @export
finbif_admin_status <- function() {
  df <- admin_status_translations
  nrows <- nrow(admin_status_translations)
  df <- df[order(df[["translated_status"]]), ]
  structure(df, row.names = seq_len(nrows), names = c("status", "code"))
}
