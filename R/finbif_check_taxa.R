#' Check FinBIF taxa
#'
#' Check that taxa are in the FinBIF database.
#'
#' @aliases fb_check_taxa
#'
#' @param taxa Character (or list of named character) vector(s). If a list each
#'   vector can have the name of a taxonomic rank (genus, species, etc.,).
#'   The elements of the vectors should be the taxa to check.
#' @param cache Logical or Integer. If `TRUE` or a number greater than zero,
#'   then data-caching will be used. If not logical then cache will be
#'   invalidated after the number of hours indicated by the argument.
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
finbif_check_taxa <- function(
  taxa,
  cache = getOption("finbif_use_cache")
) {
  taxa_list <- as.list(taxa)
  taxa_list_names <- names(taxa_list)
  taxa_list_names <- tolower(taxa_list_names)
  has_names <- length(taxa_list_names) > 0L

  for (i in seq_along(taxa_list)) {
    taxa_list_name <- character()

    if (has_names) {
      taxa_list_name <- taxa_list_names[[i]]
    }

    taxa_list_name_length <- length(taxa_list_name)
    no_name <- identical(taxa_list_name_length, 0L)

    taxa_i <- taxa_list[[i]]
    taxa_names <- taxa_i

    for (j in seq_along(taxa_i)) {
      id <- NA_character_

      taxon <- taxa_i[[j]]
      resp <- finbif_taxa(taxon, cache = cache)

      if (length(resp[["content"]]) > 0L) {
        content <- resp[["content"]][[1L]]
        check_rank_obj <- list(name = taxa_list_name, rank = content)

        if (no_name || check_rank(check_rank_obj)) {
          id <- content[["id"]]
        }

      }

      taxa_i[[j]] <- id
    }

    names(taxa_i) <- taxa_names
    taxa_list[[i]] <- taxa_i

  }

  if (has_names) {
    names(taxa_list) <- taxa_list_names
  }

  structure(taxa_list, class = c("list", "finbif_taxa_list"))
}

#' @noRd
check_rank <- function(obj) {
  rank <- sub("MX.", "", obj[[c("rank", "taxonRank")]])
  identical(obj[["name"]], rank)
}
