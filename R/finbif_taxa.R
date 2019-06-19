#' Search the FinBIF taxa
#'
#' Search the FinBIF database for taxon.
#'
#' @param name Character. The name of a taxon to search for.
#' @param n Integer. Maximum number of matches to return.
#' @param type Character. Type of match to make. Must be one of `exact`,
#'   `partial` or `likely`.
#' @return A `finbif_api` object.
#' @export

finbif_taxa <- function(name, n = 1, type = c("exact", "partial", "likely")) {
  path <- "v0/taxa/search"
  type <- match.arg(type)
  query <- list(
    query = name,
    matchType = type,
    limit = n
  )
  finbif_api_get(path, query)
}
