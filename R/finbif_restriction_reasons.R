#' Data restriction reasons
#'
#' Display the reasons data in the FinBIF database have security restrictions.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display data restriction reasons
#' finbif_restriction_reasons()
#' }
#' @export
finbif_restriction_reasons <- function() {
  structure(
    restriction_reason,
    row.names = seq_len(nrow(restriction_reason)),
    names = c("reason", "description")
  )
}
