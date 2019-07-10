#' Download FinBIF occurrence records
#'
#' Download filtered occurrence data from FinBIF as a `data.frame`.
#'
#' @param ... Character vectors or list of character vectors. Taxa of records
#'   to download.
#' @inheritParams finbif_records
#' @param check_taxa Logical. Check first that taxa are in the FinBIF database.
#'   If true only records that match known taxa (have a valid taxon ID) are
#'   returned.
#' @return A `data.frame`.
#' @examples \dontrun{
#'
#' # Get recent occurrence data for taxon
#' finbif_occurrence("Cygnus cygnus")
#'
#' # Specify the number of records
#' finbif_occurrence("Cygnus cygnus", n = 100)
#'
#' # Get multiple taxa
#' finbif_occurrence("Cygnus cygnus", "Ursus arctos")
#'
#' # Filter the record
#' finbif_occurrence(
#'   species = "Cygnus cygnus",
#'   filters = list(coordinateAccuracyMax = 100))
#' )
#'
#' }
#' @importFrom utils hasName
#' @export

finbif_occurrence <- function(..., filters, fields, n = 10, page = 1,
  check_taxa = TRUE) {

  taxa <- list(...)

  if (check_taxa) {
    taxa <- if (...length() > 1L || !utils::hasName(taxa, "taxa")) {
      finbif_check_taxa(taxa)
    } else {
      finbif_check_taxa(...)
    }
    taxa <- list(taxon_id = paste(unlist(taxa), collapse = ","))
  } else {
    taxa <- list(taxon_name = paste(unlist(taxa), collapse = ","))
  }

  if (missing(filters)) filters <- NULL
  filters <- c(taxa, filters)

  records <- finbif_records(filters, fields, n, page)

  df <- as.data.frame(records)
  df <- df[intersect(row.names(field_translations), names(df))]

  structure(
    df,
    names     = field_translations[names(df), "translated_field"],
    class     = c("finbif_occ", "data.frame"),
    nrec_dnld = attr(records, "nrec_dnld", TRUE),
    nrec_avl  = attr(records, "nrec_avl", TRUE)
  )
}
