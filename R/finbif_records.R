#' Get FinBIF records
#'
#' Download filtered records from FinBIF.
#'
#' @param fields Character vector. Columns to return.
#' @param filters List of named character vectors. Filters to apply to records.
#' @return Data.frame.
#' @export

finbif_records <- function(fields, filters = NULL) {
  path <- "v0/warehouse/query/list"
  fields <- paste(fields, collapse = ",")
  filters <- lapply(filters, paste, collapse = ",")
  query <- list(
    page = 1,
    pageSize = 1,
    selected = fields
  )
  query <- c(query, filters)
  finbif_api_get(path, query)
}
