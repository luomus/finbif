#' Check FinBIF taxa
#'
#' Check that taxa are in the FinBIF database.
#'
#' @aliases fb_check_taxa
#'
#' @param taxa Character (or list of named character) vector(s). If a list each
#'   vector can have the name of a taxonomic rank (genus, species, etc.,).
#'   The elements of the vectors should be the taxa to check.
#' @param cache Logical. Use cached data.
#' @return An object of class `finbif_taxa`. A list with the same form as
#'   `taxa`.
#' @examples \dontrun{
#'
#' # Check a scientific name
#' finbif_check_taxa("Cygnus cygnus")
#'
#' # Check a common name
#' finbif_check_taxa("Whooper swan")
#'
#' # Check a genus
#' finbif_check_taxa("Cygnus")
#'
#' # Check a list of taxa
#' finbif_check_taxa(
#'   list(
#'     species = c("Cygnus cygnus", "Ursus arctos"),
#'     genus   = "Betula"
#'   )
#' )
#' }
#' @export

finbif_check_taxa <- function(taxa, cache = getOption("finbif_use_cache")) {
  taxa <- as.list(taxa)
  out <- taxa
  for (i in seq_along(taxa)) {
    rank <- tolower(names(taxa)[[i]])
    names(out[[i]]) <- taxa[[i]]
    for (j in seq_along(taxa[[i]])) {
      resp <- finbif_taxa(taxa[[i]][[j]], cache = cache)
      if (length(resp[["content"]])) {
        rank_ <- tolower(gsub("MX.", "", resp[["content"]][[1]][["taxonRank"]]))
        if (identical(rank, rank_) || identical(rank, character())) {
          out[[i]][[j]] <- resp[["content"]][[1]][["id"]]
        } else {
          out[[i]][[j]] <- NA_character_
        }
      } else {
        out[[i]][[j]] <- NA_character_
      }
    }
  }
  structure(out, class = c("list", "finbif_taxa_list"))
}
