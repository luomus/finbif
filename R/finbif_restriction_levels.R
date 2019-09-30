#' Data restriction levels
#'
#' Display the levels of data security restrictions in the FinBIF database.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display data restriction levels
#' finbif_restriction_levels()
#' }
#' @export
finbif_restriction_levels <- function() {
  structure(
    restriction_level,
    row.names = seq_len(nrow(restriction_level)),
    names = c("level", "description")
  )
}
