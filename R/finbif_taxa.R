#' Search the FinBIF taxa
#'
#' Search the FinBIF database for taxon.
#'
#' @aliases fb_taxa
#'
#' @param name Character. The name or ID of a taxon. Or, for functions other
#'    than `finbif_taxa` a `finbif_taxa` object.
#' @param n Integer. Maximum number of matches to return. For types "exact" and
#'   "likely" only one taxon will be returned.
#' @param type Character. Type of match to make. Must be one of `exact`,
#'   `partial` or `likely`.
#' @param cache Logical. Use cached data.
#' @param locale Character. One of the supported two-letter ISO 639-1 language
#'   codes. Current supported languages are English, Finnish, Swedish, Russian,
#'   and Sámi (Northern). For data where more than one language is available
#'   the language denoted by `locale` will be preferred while falling back to
#'   the other languages in the order indicated above.
#' @return For `finbif_taxa` a `finbif_taxa` object. Otherwise, a character
#'    vector.
#' @examples \dontrun{
#'
#' # Search for a taxon
#' finbif_taxa("Ursus arctos")
#'
#' # Use partial matching
#' finbif_taxa("Ursus", n = 10, "partial")
#'
#' # Get Sámi (Northern) name of Eurasian Eagle-owl
#' common_name("Bubo bubo", "se")
#'
#' # Get scientific name of "Otter"
#' scientific_name("Otter")
#'
#' # Get taxon identifier of "Otter"
#' taxon_id("Otter")
#'
#' }
#' @export

finbif_taxa <- function(
  name, n = 1, type = c("exact", "partial", "likely"),
  cache = getOption("finbif_use_cache")
) {
  path <- "taxa/search"
  type <- match.arg(type)
  query <- list(query = name, matchType = type, limit = n)
  structure(
    api_get(list(path = path, query = query, cache = cache)),
    class = c("finbif_taxa", "finbif_api")
  )
}

taxon_attribute <- function(obj) {
  taxon <- finbif_taxa(obj[["taxon"]], n = 1, type = "exact")
  taxon <- taxon[["content"]]
  taxon <- taxon[[1L]]
  with_locale(taxon[[obj[["which"]]]], obj[["locale"]])
}

#' @export
#' @rdname finbif_taxa
common_name <- function(name, locale = getOption("finbif_locale")) {
  obj <- list(taxon = name, which = "vernacularName", locale = locale)
  taxon_attribute(obj)
}

#' @export
#' @rdname finbif_taxa
scientific_name <- function(name) {
  obj <- list(taxon = name, which = "scientificName")
  taxon_attribute(obj)
}

#' @export
#' @rdname finbif_taxa
taxon_id <- function(name) {
  obj <- list(taxon = name, which = "id")
  taxon_attribute(obj)
}
