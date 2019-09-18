#' FinBIF taxonomic ranks
#'
#' Display taxonomic ranks used in the FinBIF database.
#'
#' @return A character vector.
#' @examples \dontrun{
#'
#' # Display the taxonomic ranks used by FinBIF
#' finbif_taxon_ranks()
#' }
#' @export
finbif_taxon_ranks <- function() unclass(taxon_rank[["rank"]])
