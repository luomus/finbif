#' IUCN red list
#'
#' Display the IUCN red list statuses used in the FinBIF database.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display the IUCN red list statuses used by FinBIF
#' finbif_red_list()
#' }
#' @export
finbif_red_list <- function() {
  df <- red_list_status
  df <- df[order(df[["translated_status"]]), ]
  structure(df, row.names = seq_len(nrow(df)), names = c("status", "code"))
}
