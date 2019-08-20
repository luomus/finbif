# records ----------------------------------------------------------------------

#' Get FinBIF records
#'
#' Download records from FinBIF.
#'
#' @param filter List of named character vectors. Filters to apply to records.
#' @param select Character vector. Variables to return. If not specified a
#'   default set of commonly used variables will be used. Use `"default_vars"`
#'   as ashortcut for this set.
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

finbif_records <- function(filter, select, n = 10, page = 1,
  count_only = FALSE, quiet = FALSE, cache = TRUE) {

  max_queries  <- 600L
  max_size <- 300L
  nmax <- max_queries * max_size

  defer_errors({

    if (n > nmax)
      deferrable_error(paste("Cannot download more than", nmax, "records"))

    # filters ==================================================================

    if (missing(filter)) {

      query <- list()

    } else {

      filter <- as.list(filter)
      finbif_filter_names <- translate(names(filter), "filter_names")

      for (i in seq_along(filter)) {
        # the filter might not exist
        if (all(filter_names[finbif_filter_names[[i]], "translated_values"]))
          filter[[i]] <- translate(filter[[i]], names(filter)[[i]])
      }

      names(filter) <- finbif_filter_names

      query <- lapply(filter, paste, collapse = ",")

    }

    # vars =====================================================================

    default_vars <- var_names[var_names[["default_var"]], ]

    if (missing(select)) {

      select <- row.names(default_vars)

    } else {

      select <- ifelse(
        select == "default_vars",
        list(default_vars[["translated_var"]]),
        select
      )
      select <- unlist(select)
      select <- translate(select, "var_names")

    }

    query[["selected"]] <- paste(select, collapse = ",")

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

    if (anyNA(ind)) for (err in x[is.na(ind)])
      deferrable_error(
        paste0(
          "Invalid name in ", gsub("_", " ", translation), ": ", err
        )
      )
    row.names(trsltn)[ind]

  }
}
