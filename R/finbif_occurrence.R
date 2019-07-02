#' Download FinBIF occurrence records
#'
#' Download filtered occurence data from FinBIF as a `data.frame`.
#'
#' @param ... Character vectors or list of character vectors. Taxa to download.
#' @inheritParams finbif_records
#' @return A `data.frame`.
#' @importFrom utils hasName
#' @export

finbif_occurrence <- function(..., filters = NULL, fields, n = 10, page = 1) {
  taxa <- list(...)
  taxa <- if (...length() > 1L || !utils::hasName(taxa, "taxa")) {
    finbif_check_taxa(taxa)
  } else {
    finbif_check_taxa(...)
  }
  taxa <- list(taxonId = paste(unlist(taxa), collapse = ","))
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
