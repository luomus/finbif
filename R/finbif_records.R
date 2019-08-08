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

  query <- if (missing(filters)) list() else translate_filters(filters)

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

  resp <- list(finbif_api_get(path, query, cache))

  n_tot <- resp[[1L]][["content"]][["total"]]
  n <- min(n, n_tot)
  multipage <- n > max_size

  if (multipage && !quiet) {
    pb <- utils::txtProgressBar(0L, floor(n / max_size), style = 3L)
    on.exit(close(pb))
  }

  i <- 1L
  query[["page"]] <- query[["page"]] + 1L
  n_pages <- n %/% query[["pageSize"]]

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

translate_filters <- function(filters) {

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

  filters[["primary_habitat"]] <-
    translate_habitat(filters[["primary_habitat"]])

  filters[["primary_secondary_habitat"]] <-
    translate_habitat(filters[["primary_secondary_habitat"]])

  filters[["taxon_rank"]] <-
    translate(filters[["taxon_rank"]], taxon_ranks, "rank")

  filters[["country"]] <-
    translate(filters[["country"]], countries, colnames(countries))

  filters[["province"]] <- translate(
    filters[["province"]], provinces, setdiff(colnames(provinces), "country")
  )

  filters[["municipality"]] <- translate(
    filters[["municipality"]], municipalities,
    setdiff(colnames(municipalities), "country")
  )

  names(filters) <-
    translate(names(filters), filter_translations, "translated_filter")

  lapply(filters, paste, collapse = ",")

}

translate_habitat <- function(habitat) {
  if (is.list(habitat)) {
    names(habitat) <-
      translate(names(habitat), habitat_types[["habitat_types"]], "code")
    habitat <- lapply(
      habitat, translate, habitat_types[["specific_habitat_types"]], "code"
    )
    habitat <- lapply(habitat, paste, collapse = ",")
    sprintf(
      "%s%s",
      names(habitat),
      ifelse(habitat == "", habitat, sprintf("[%s]", habitat))
    )
  } else {
    translate(habitat, habitat_types[["habitat_types"]], "code")
  }
}

translate <- function(x, translation, col) {
  if (is.null(x)) return(NULL)
  ind <- rep(NA_integer_, length(x))
  for (i in col) {
    ind_ <- match(x, translation[[i]])
    ind <- ifelse(is.na(ind_), ind, ind_)
  }
  if (anyNA(ind)) stop("Invalid name in ", deparse(substitute(translation)))
  row.names(translation)[ind]
}
