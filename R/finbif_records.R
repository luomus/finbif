#' Get FinBIF records
#'
#' Download records from FinBIF.
#'
#' @param filters List of named character vectors. Filters to apply to records.
#' @param fields Character vector. Columns to return.
#' @param n Integer. How many records to download.
#' @param page Integer. Which page of records to start downloading from.
#' @param count_only Logical. Only return the number of records available.
#' @return A `finbif_api` or `finbif_api_list` object.
#' @examples \dontrun{
#'
#' # Get the last 100 records from FinBIF
#' finbif_records(n = 100)
#' }
#' @export

finbif_records <- function(filters, fields, n = 10, page = 1,
  count_only = FALSE) {

  path <- "v0/warehouse/query/"

  max_queries  <- 10L
  max_size <- 100L
  nmax <- max_queries * max_size
  if (n > nmax) stop("Cannot download more than ", nmax, " records")

  if (missing(filters)) {
    query <- list()
  } else {
    query <- translate_filters(as.list(filters))
  }

  if (missing(fields)) {
    fields <- row.names(
      field_translations[field_translations[["default_field"]], ]
    )
  } else {
    fields <- translate_fields(fields)
  }

  query[["selected"]] <- paste(fields, collapse = ",")

  if (count_only) {

    path <- paste0(path, "count")
    return(finbif_api_get(path, query))

  } else {

    path <- paste0(path, "list")
    query[["page"]] <- page
    query[["pageSize"]] <- min(n, max_size)

  }

  resp <- list()
  i <- 1L

  message("Downloading page ", query[["page"]])
  resp[[i]] <- finbif_api_get(path, query)

  n_tot <- resp[[1]][["content"]][["total"]]
  n <- min(n, n_tot)

  while (max_size * i < n) {
    Sys.sleep(1)

    message("Downloading page ", resp[[i]][["content"]][["nextPage"]])
    i <- i + 1L
    query[["page"]] <- query[["page"]] + 1L
    query[["pageSize"]] <- if (max_size * i > n) n %% max_size
    resp[[i]] <- finbif_api_get(path, query)

  }

  structure(resp, class = "finbif_api_list", nrec_dnld = n, nrec_avl = n_tot)

}

translate_filters <- function(filters) {
  ind <- match(names(filters), filter_translations[["translated_filter"]])
  if (anyNA(ind)) stop("Invalid filter name")
  filters[["informal_group"]] <- translate_informal_groups(
    filters[["informal_group"]]
  )
  filters[["informal_group_reported"]] <- translate_informal_groups(
    filters[["informal_group_reported"]]
  )
  filters[["administrative_status"]] <- translate_admin_status(
    filters[["administrative_status"]]
  )
  names(filters) <- row.names(filter_translations)[ind]
  lapply(filters, paste, collapse = ",")
}

translate_informal_groups <- function(groups) {
  if (is.null(groups)) return(NULL)
  ind <- match(groups, informal_groups[["name"]])
  if (anyNA(ind)) stop("Invalid informal group")
  row.names(informal_groups)[ind]
}

translate_admin_status <- function(statuses) {
  if (is.null(statuses)) return(NULL)
  ind <- match(statuses, admin_status_translations[["translated_status_code"]])
  if (anyNA(ind)) stop("Invalid administrative status")
  row.names(admin_status_translations)[ind]
}

translate_fields <- function(fields) {
  ind <- match(fields, field_translations[["translated_field"]])
  if (anyNA(ind)) stop("Invalid field name")
  row.names(field_translations)[ind]
}
