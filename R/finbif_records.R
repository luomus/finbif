# records ----------------------------------------------------------------------

#' Get FinBIF records
#'
#' Download records from FinBIF.
#'
#' @param filter List of named character vectors. Filters to apply to records.
#' @param select Character vector. Variables to return. If not specified a
#'   default set of commonly used variables will be used. Use `"default_vars"`
#'   as a shortcut for this set.
#' @param order_by Character vector. Variables to order records by before they
#'   are returned. Most, though not all, variables can be used to order records
#'   before they are returned. Ordering is ascending by default. To return in
#'   descending order append a `-` to the front of the variable (e.g.,
#'   `"-date_start"`). Default order is `"-date_start"` > `"-load_data"` >
#'   `"reported_name"`.
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

finbif_records <- function(
  filter, select, order_by, n = 10, page = 1, count_only = FALSE, quiet = FALSE,
  cache = TRUE
) {

  max_queries  <- 2000L
  max_size <- 1000L
  nmax <- max_queries * max_size

  defer_errors({

    if (n > nmax)
      deferrable_error(paste("Cannot download more than", nmax, "records"))

    # filter ===================================================================

    if (missing(filter)) {

      query <- list()

    } else {

      query <- lapply(parse_filters(filter), paste, collapse = ",")

    }

    # select ===================================================================

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
      select_vars <- var_names[var_names[["select"]], ]
      for (var in names(var_names))
        class(select_vars[[var]]) <- class(var_names[[var]])
      select <-
        translate(select, "select_vars", list(select_vars = select_vars))

    }

    query[["selected"]] <- paste(select, collapse = ",")

    # order ====================================================================

    if (missing(order_by)) {

      query[["orderBy"]] <- NULL

    } else {

      desc_order <- grepl("^-", order_by)
      order_by <- sub("^-", "", order_by)
      order_vars <- var_names[var_names[["order"]], ]
      for (var in names(var_names))
        class(order_vars[[var]]) <- class(var_names[[var]])
      order_by <-
        translate(order_by, "order_vars", list(order_vars = order_vars))
      order_by[desc_order] <- paste(order_by, "DESC")
      query[["orderBy"]] <- paste(order_by, collapse = ",")

    }

  })

  # request ====================================================================

  path <- "warehouse/query/"

  if (count_only) {

    path <- paste0(path, "count")
    return(finbif_api_get(path, query, cache))

  }

  path <- paste0(path, "list")
  query[["page"]] <- page
  query[["pageSize"]] <- min(n, max_size)

  resp <- list(
    structure(
      finbif_api_get(path, query, cache),
      class = c("finbif_records", "finbif_api"),
      select = unique(select)
    )
  )

  n_tot <- resp[[1L]][["content"]][["total"]]
  n <- min(n, n_tot)

  resp <- get_extra_pages(resp, n, max_size, quiet, path, query, cache, select)

  structure(
    resp, class = c("finbif_records_list", "finbif_api_list"), nrec_dnld = n,
    nrec_avl = n_tot, select = unique(select)
  )

}

# record pagination ------------------------------------------------------------

get_extra_pages <-
  function(resp, n, max_size, quiet, path, query, cache, select) {

    multipage <- n > max_size

    if (multipage && !quiet) {
      pb_head("Fetching data")
      pb <- utils::txtProgressBar(0L, floor(n / max_size), style = 3L)
      on.exit(close(pb))
    }

    i <- 1L
    query[["page"]] <- query[["page"]] + 1L
    n_pages <- n %/% query[["pageSize"]]

    # Pausing between requests is important if many request will be made
    sleep <- ifelse(n_pages > 10, 0L, 1L)

    while (multipage) {

      Sys.sleep(sleep)
      if (!quiet) utils::setTxtProgressBar(pb, i)
      i <- i + 1L

      if (query[["page"]] > n_pages) {
        excess_records <- n %% query[["pageSize"]]
        last_record <- query[["pageSize"]] * n_pages
        if (last_record == n) break
        query[["pageSize"]] <-
          get_next_lowest_factor(last_record, excess_records)
        query[["page"]] <- last_record / query[["pageSize"]] + 1L
        n_pages <- n %/% query[["pageSize"]]
      }

      resp[[i]] <- structure(
        finbif_api_get(path, query, cache),
        class = c("finbif_records", "finbif_api"),
        select = unique(select)
      )

      query[["page"]] <- query[["page"]] + 1L

    }

    resp

  }

# parsing filters --------------------------------------------------------------

parse_filters <- function(filter) {

  filter <- as.list(filter)
  finbif_filter_names <- translate(names(filter), "filter_names")

  for (i in seq_along(filter)) {

    # the filter might not exist
    if (is.na(finbif_filter_names[[i]])) next

    if (filter_names[finbif_filter_names[[i]], "translated_values"])
      filter[[i]] <- translate(filter[[i]], names(filter)[[i]])

    if (grepl("^(not_){0,1}collection$", names(filter)[[i]])) {

      if (inherits(filter[[i]], "finbif_collections")) {

        filter[[i]] <- row.names(filter[[i]])

      } else {

        env <- list()

        env[[names(filter)[[i]]]] <- finbif_collections(
          select = NA, supercollections = TRUE, nmin = NA
        )

        for (cl in c("id", "collection_name", "abbreviation"))
          class(env[[names(filter)[[i]]]][[cl]]) <- "translation"

        filter[[i]] <- translate(filter[[i]], names(filter)[[i]], env)

      }
    }

    if (
      identical(filter_names[finbif_filter_names[[i]], "class"], "coords")
    )
      filter[[i]] <- do.call(finbif_coords, as.list(filter[[i]]))

    if (identical(filter_names[finbif_filter_names[[i]], "class"], "date"))
      filter[[i]] <- do.call(
        finbif_dates, c(list(names(filter)[[i]]), as.list(filter[[i]]))
      )

    filter[[i]] <- paste(
      filter[[i]], collapse = filter_names[finbif_filter_names[[i]], "sep"]
    )

  }

  names(filter) <- finbif_filter_names

  filter

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
    # multilevel filters have data.frames a level below
    if (!is.data.frame(trsltn)) trsltn <- trsltn[[1L]]
    for (i in trsltn) {
      if (inherits(i, "translation")) {
        ind_ <- match(tolower(x), tolower(i))
        ind <- ifelse(is.na(ind_), ind, ind_)
      }
    }

    if (anyNA(ind)) for (err in x[is.na(ind)])
      deferrable_error(
        paste0("Invalid name in ", gsub("_", " ", translation), ": ", err)
      )
    row.names(trsltn)[ind]

  }
}
