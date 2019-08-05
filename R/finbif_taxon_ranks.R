#' FinBIF taxonomic ranks
#'
#' Display taxonomic ranks used in the FinBIF database.
#'
#' @return A data.frame.
#' @examples \dontrun{
#'
#' # Display the taxonomic ranks used by FinBIF
#' finbif_taxon_ranks()
#' }
#' @export
finbif_taxon_ranks <- function() taxon_ranks[["rank"]]
