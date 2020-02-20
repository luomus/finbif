#' Search the FinBIF taxa
#'
#' Search the FinBIF database for taxon.
#'
#' @param name Character. The name of a taxon to search for.
#' @param n Integer. Maximum number of matches to return. For types "exact" and
#'   "likely" a maximum of one taxon will be returned.
#' @param type Character. Type of match to make. Must be one of `exact`,
#'   `partial` or `likely`.
#' @param cache Logical. Use cached data.
#' @return A `finbif_api` object.
#' @examples \dontrun{
#'
#' # Search for a taxon
#' finbif_taxa("Ursus arctos")
#'
#' # Use partial matching
#' finbif_taxa("Ursus", n = 10, "partial")
#' }
#' @export

finbif_taxa <- function(
  name, n = 1, type = c("exact", "partial", "likely"),
  cache = getOption("finbif_use_cache")
) {
  path <- "taxa/search"
  type <- match.arg(type)
  query <- list(query = name, matchType = type, limit = n)
  api_get(path, query, cache)
}
