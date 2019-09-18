#' FinBIF life stages
#'
#' Display organism life stages used in the FinBIF database.
#'
#' @return A character vector.
#' @examples \dontrun{
#'
#' # Display list of life stages
#' finbif_life_stages()
#' }
#' @export
finbif_life_stages <- function() unclass(life_stage[["stage"]])
