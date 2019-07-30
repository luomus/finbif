#' Get FinBIF records
#'
#' Download records from FinBIF.
#'
#' @param filters List of named character vectors. Filters to apply to records.
#' @param fields Character vector. Columns to return. If not specified a default
#'   set of commonly used fields will be used. Use `"default_fields"` as a
#'   shortcut for this set.
#' @param n Integer. How many records to download.
#' @param page Integer. Which page of records to start downloading from.
#' @param count_only Logical. Only return the number of records available.
#' @param quiet Logical. Suppress the progress indicator for multipage
#'   downloads.
#' @param cache Logical. Use cached data.
#' @return A `finbif_api` or `finbif_api_list` object.
#' @examples \dontrun{
#'
#' # Get the last 100 records from FinBIF
#' finbif_records(n = 100)
#' }
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

finbif_records <- function(filters, fields, n = 10, page = 1,
  count_only = FALSE, quiet = FALSE, cache = TRUE) {

  path <- "v0/warehouse/query/"

  max_queries  <- 600L
  max_size <- 300L
  nmax <- max_queries * max_size
  if (n > nmax) stop("Cannot download more than ", nmax, " records")

  if (missing(filters)) {

    query <- list()

  } else {

    filters <- as.list(filters)
    filters[["informal_group"]] <- translate(
      to_sentence_case(filters[["informal_group"]]), informal_groups, "name"
    )
    filters[["informal_group_reported"]] <- translate(
      to_sentence_case(filters[["informal_group_reported"]]), informal_groups,
      "name"
    )
    filters[["administrative_status"]] <- translate(
      filters[["administrative_status"]], admin_status_translations,
      "translated_status_code"
    )
    filters[["red_list_status"]] <- translate(
      filters[["red_list_status"]], redlist_status_translations,
      "translated_status_code"
    )
    names(filters) <-
      translate(names(filters), filter_translations, "translated_filter")
    query <- lapply(filters, paste, collapse = ",")

  }

  default_fields <- field_translations[field_translations[["default_field"]], ]

  if (missing(fields)) {
    fields <- row.names(default_fields)
  } else {
    fields <- ifelse(
      fields == "default_fields",
      list(default_fields[["translated_field"]]),
      fields
    )
    fields <- unlist(fields)
    fields <- translate(fields, field_translations, "translated_field")
  }

  query[["selected"]] <- paste(fields, collapse = ",")

  if (count_only) {

    path <- paste0(path, "count")
    return(finbif_api_get(path, query, cache))

  } else {

    path <- paste0(path, "list")
    query[["page"]] <- page
    query[["pageSize"]] <- min(n, max_size)

  }

  resp <- list()
  i <- 1L

  resp[[i]] <- finbif_api_get(path, query, cache)
  query[["page"]] <- query[["page"]] + 1L

  n_tot <- resp[[1L]][["content"]][["total"]]
  n <- min(n, n_tot)
  n_pages <- n %/% query[["pageSize"]]
  multipage <- n > max_size

  if (multipage && !quiet) {
    pb <- utils::txtProgressBar(0L, floor(n / max_size), style = 3L)
    on.exit(close(pb))
  }

  while (multipage) {

    Sys.sleep(1L)
    utils::setTxtProgressBar(pb, i)
    i <- i + 1L

    if (query[["page"]] > n_pages) {
      excess_records <- n %% query[["pageSize"]]
      last_record <- query[["pageSize"]] * n_pages
      if (last_record == n) break
      query[["pageSize"]] <- get_next_lowest_factor(last_record, excess_records)
      query[["page"]] <- last_record / query[["pageSize"]] + 1L
      n_pages <- n %/% query[["pageSize"]]
    }

    resp[[i]] <- finbif_api_get(path, query, cache)
    query[["page"]] <- query[["page"]] + 1L

  }

  structure(resp, class = "finbif_api_list", nrec_dnld = n, nrec_avl = n_tot)

}

translate <- function(x, y, z) {
  if (is.null(x)) return(NULL)
  ind <- match(x, y[[z]])
  if (anyNA(ind)) stop("Invalid name in ", deparse(substitute(x)))
  row.names(y)[ind]
}
