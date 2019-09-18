#' FinBIF data sources
#'
#' Display the information system data sources used by FinBIF.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display data sources
#' finbif_sources()
#' }
#' @export
finbif_sources <-
  function() structure(source, row.names = seq_len(nrow(source)))
