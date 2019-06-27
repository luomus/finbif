#' Get FinBIF records
#'
#' Download filtered records from FinBIF.
#'
#' @param filters List of named character vectors. Filters to apply to records.
#' @param fields Character vector. Columns to return.
#' @param n Integer. How many records to download per page.
#' @param page Integer. Which page of records to download.
#' @return A `finbif_api` object.
#' @export

finbif_records <- function(filters = NULL, fields, n = 10, page = 1) {
  path <- "v0/warehouse/query/list"
  if (missing(fields)) fields <-
    row.names(field_translations[field_translations[["default_field"]], ])
  fields <- paste(fields, collapse = ",")
  filters <- lapply(filters, paste, collapse = ",")
  query <- list(
    page = page,
    pageSize = n,
    selected = fields
  )
  query <- c(query, filters)
  finbif_api_get(path, query)
}
