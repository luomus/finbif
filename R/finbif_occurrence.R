#' Download FinBIF occurrence records
#'
#' Download filtered occurence data from FinBIF as a `data.frame`.
#'
#' @inheritParams finbif_check_taxa
#' @inheritParams finbif_records
#' @return A `data.frame`.
#' @export

finbif_occurrence <- function(taxa, filters = NULL, fields, n = 10, page = 1) {
  taxa <- finbif_check_taxa(taxa)
  taxa <- list(taxonId = paste(unlist(taxa), collapse = ","))
  filters <- c(taxa, filters)
  records <- finbif_records(filters, fields, n, page)
  df <- as.data.frame(records)
  df <- df[intersect(row.names(field_translations), names(df))]
  names(df) <- field_translations[names(df), "translated_field"]
  df
}
