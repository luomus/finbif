#' Check FinBIF taxa
#'
#' Check that taxa are in the FinBIF database.
#'
#' @param taxa Named list of vectors. Each vector should have the name of a
#'   taxonomic rank (genus, species, etc.,). The elements of the vectors should
#'   be the taxa tp check and be of type character.
#' @return A list.
#' @export

finbif_check_taxa <- function(taxa) {
  out <- taxa
  for (i in seq_along(taxa)) {
    rank <- tolower(names(taxa)[[i]])
    names(out[[i]]) <- taxa[[i]]
    for (j in seq_along(taxa[[i]])) {
      resp <- finbif_taxa(taxa[[i]][[j]])
      if (length(resp$content)) {
        rank_ <- tolower(gsub("MX.", "", resp$content[[1]]$taxonRank))
        out[[i]][[j]] <-
          if (identical(rank, rank_)) resp$content[[1]]$id else NA_character_
      } else {
        out[[i]][[j]] <- NA_character_
      }
    }
  }
  out
}