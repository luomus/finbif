# records ----------------------------------------------------------------------

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

  max_queries  <- 600L
  max_size <- 300L
  nmax <- max_queries * max_size

  defer_errors({

    if (n > nmax)
      deferrable_error(paste("Cannot download more than", nmax, "records"))

    # filters ==================================================================

    if (missing(filters)) {

      query <- list()

    } else {

      filters <- as.list(filters)
      translated_filter_names <- translate(names(filters), "filter_names")

      for (filter in names(filters)) {
        should_translate <- filter_names[["translated_filter"]] == filter
        should_translate <- filter_names[should_translate, "translated_values"]
        # the filter might not exist
        if (should_translate && length(should_translate))
          filters[[filter]] <- translate(filters[[filter]], filter)
      }

      names(filters) <- translated_filter_names

      query <- lapply(filters, paste, collapse = ",")

    }

    # fields ===================================================================

    default_fields <- field_names[field_names[["default_field"]], ]

    if (missing(fields)) {

      fields <- row.names(default_fields)

    } else {

      fields <- ifelse(
        fields == "default_fields",
        list(default_fields[["translated_field"]]),
        fields
      )
      fields <- unlist(fields)
      fields <- translate(fields, "field_names")

    }

    query[["selected"]] <- paste(fields, collapse = ",")

  })

  # request ====================================================================

  path <- "warehouse/query/"

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

# translation ------------------------------------------------------------------

translate <- function(x, translation, pos = -1) {
  trsltn <- get(translation, pos)
  # Some filters have multi-level values to translate (e.g., primary_habitat)
  if (is.list(x)) {

    names(x) <- translate(names(x), names(trsltn)[[1L]], trsltn)
    x <- lapply(x, translate, names(trsltn)[[2L]], trsltn)
    x <- lapply(x, paste, collapse = ",")
    sprintf("%s%s", names(x), ifelse(x == "", x, sprintf("[%s]", x)))

  } else {

    ind <- rep(NA_integer_, length(x))
    for (i in trsltn) {
      if (inherits(i, "translation")) {
        ind_ <- match(tolower(x), tolower(i))
        ind <- ifelse(is.na(ind_), ind, ind_)
      }
    }

    if (anyNA(ind))
      deferrable_error(paste("Invalid name in", gsub("_", " ", translation)))
    row.names(trsltn)[ind]

  }
}
