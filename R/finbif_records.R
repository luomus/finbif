# records ----------------------------------------------------------------------

#' Get FinBIF records
#'
#' Download records from FinBIF.
#'
#' @aliases fb_records
#'
#' @param filter List of named character vectors. Filters to apply to records.
#' @param select Character vector. Variables to return. If not specified, a
#'   default set of commonly used variables will be used. Use `"default_vars"`
#'   as a shortcut for this set. Variables can be deselected by prepending a `-`
#'   to the variable name. If only deselects are specified the default set of
#'   variables without the deselection will be returned.
#' @param order_by Character vector. Variables to order records by before they
#'   are returned. Most, though not all, variables can be used to order records
#'   before they are returned. Ordering is ascending by default. To return in
#'   descending order append a `-` to the front of the variable (e.g.,
#'   `"-date_start"`). Default order is `"-date_start"` > `"-load_data"` >
#'   `"reported_name"`.
#' @param aggregate Character. If missing, returns full records. If one or more
#'   of `"records"`, `"species"`, `"taxa"`, or `"events"` aggregates
#'   combinations of the selected variables by counting records, species, taxa,
#'   or events. Aggregation by event cannot be done in combination with any of
#'   the other aggregation types.
#' @param sample Logical. If `TRUE` randomly sample the records from the FinBIF
#'   database.
#' @param n Integer. How many records to download/import.
#' @param page Integer. Which page of records to start downloading from.
#' @param count_only Logical. Only return the number of records available.
#' @param quiet Logical. Suppress the progress indicator for multipage
#'   downloads.
#' @param cache Logical. Use cached data.
#' @param dwc Logical. Use Darwin Core (or Darwin Core style) variable names.
#' @param seed Integer. Set a seed for randomly sampling records. Note that the
#'   server currently ignores seed setting and this argument currently has
#'   little effect.
#' @param df Logical. Should the data.frame representation of the records be
#'   returned as an attribute?
#' @return A `finbif_api` or `finbif_api_list` object.
#' @examples \dontrun{
#'
#' # Get the last 100 records from FinBIF
#' finbif_records(n = 100)
#' }
#' @importFrom utils hasName txtProgressBar setTxtProgressBar
#' @export

finbif_records <- function(
  filter, select, order_by, aggregate, sample = FALSE, n = 10, page = 1,
  count_only = FALSE, quiet = FALSE, cache = getOption("finbif_use_cache"),
  dwc = FALSE, seed, df = FALSE
) {

  max_queries        <- 2000L
  max_size           <- 1000L
  nmax               <- max_queries * max_size
  n                  <- as.integer(n)
  var_type           <- col_type_string(dwc)

  defer_errors({

    check_n(n, nmax)

    # aggregation ==============================================================

    aggregate <- infer_aggregation(aggregate)

    # filter ===================================================================

    if (missing(filter)) {

      query <- list()

    } else {

      query <- lapply(parse_filters(filter), paste, collapse = ",")

    }

    # select ===================================================================

    select <- infer_selection(aggregate, select, var_type)

    select_param <- switch(aggregate[[1L]], none = "selected", "aggregateBy")

    query[[select_param]] <- paste(select[["query"]], collapse = ",")

    # order ====================================================================

    if (missing(order_by)) {

      query[["orderBy"]] <- NULL

    } else {

      desc_order <- grepl("^-", order_by)
      order_by <- sub("^-", "", order_by)
      order_vars <- var_names[var_names[["order"]], var_type, drop = FALSE]
      class(order_vars[[var_type]]) <- class(var_names[[var_type]])
      order_by <-
        translate(order_by, "order_vars", list(order_vars = order_vars))
      order_by[desc_order] <- paste(order_by[desc_order], "DESC")
      query[["orderBy"]] <- paste(order_by, collapse = ",")

    }

    if (sample) {
      query[["orderBy"]] <- paste(
        if (missing(seed)) "RANDOM" else paste0("RANDOM:", as.integer(seed)),
        query[["orderBy"]],
        sep = c("", ",")[[length(query[["orderBy"]]) + 1L]]
      )
    }

  })

  ans <- request(
    filter, select[["query"]], sample, n, page, count_only, quiet, cache, query,
    max_size, select[["user"]], select[["record_id_selected"]], dwc, aggregate,
    df
  )

  if (df && !count_only) {
    ind <- length(ans)
    attr(ans[[ind]], "df") <- as.data.frame(ans[[ind]])
  }

  ans

}

# aggregation ------------------------------------------------------------------

infer_aggregation <- function(aggregate) {

  if (missing(aggregate)) {
     aggregate <- "none"
  }

  aggregate <- match.arg(
    aggregate, c("none", "records", "species", "taxa", "events"), TRUE
  )

  cond <- "events" %in% aggregate && length(aggregate) > 1L

  if (cond) {
    deferrable_error(
      "Aggregating by events cannot by combined with other aggregations"
    )
  }

  aggregate

}

# selection --------------------------------------------------------------------

infer_selection <- function(aggregate, select, var_type) {

  date_time_vars <- var_names[var_names[["date"]], ]
  default_vars <- var_names["unit.linkings.taxon.scientificName", ]
  select_type <- "aggregate"

  if (identical(aggregate, "events")) {

    default_vars <- var_names["gathering.gatheringId", ]
    select_type <- "aggregate_events"

  }

  if (identical(aggregate, "none")) {

    default_vars <- var_names[var_names[["default_var"]], ]
    select_type <- "select"

  }

  if (missing(select)) {

    select <- row.names(default_vars)
    select_ <- default_vars[[var_type]]
    record_id_selected <- FALSE

    if (identical(aggregate, "none")) {
      # Missing 'select' implies default selection which implies date-time calc
      # needed
      select <- unique(c(select, row.names(date_time_vars)))
      record_id_selected <- TRUE
    }

  } else {

    deselect <- substring(grep("^-", select, value = TRUE), 2L)
    if (identical(length(deselect), length(select))) select <- "default_vars"
    select <- grep("^-", select, value = TRUE, invert = TRUE)
    select <- ifelse(
      select == "default_vars", list(default_vars[[var_type]]), select
    )
    select <- unlist(select)
    select <- setdiff(select, deselect)
    select_ <- select

    record_id_selected <- var_names["unit.unitId", var_type] %in% select

    if (!record_id_selected && identical(aggregate, "none")) {
      select <- c(var_names["unit.unitId", var_type], select)
    }

    vars <-  c("date_time", "eventDateTime", "duration", "samplingEffort")
    date_time <- any(vars %in% select)

    if (date_time) {
      select <- unique(c(select, date_time_vars[[var_type]]))
    }

    select_vars <- var_names[var_names[[select_type]], var_type, drop = FALSE]
    class(select_vars[[var_type]]) <- class(var_names[[var_type]])
    select <- translate(
      select, "select_vars", list(select_vars = select_vars)
    )

    vars_computed_from_id <- grepl("^computed_var_from_id", select)

    if (any(vars_computed_from_id)) {

      vars_computed_from_id <- var_names[select[vars_computed_from_id], ]

      for (i in seq_len(nrow(vars_computed_from_id))) {

        ind <- match(row.names(vars_computed_from_id)[[i]], select)
        computed_var <- vars_computed_from_id[i, "translated_var"]
        id_var <- paste0(computed_var, "_id")
        select[[ind]] <- translate(
          id_var, "select_vars", list(select_vars = select_vars)
        )

      }

    }

  }

  # Can't query the server for vars that are computed after download
  select <- select[!grepl("^computed_var", select)]

  list(query = select, user = select_, record_id_selected = record_id_selected)

}

# request ----------------------------------------------------------------------

request <- function(
  filter, select, sample, n, page, count_only, quiet, cache, query, max_size,
  select_, record_id_selected, dwc, aggregate, df
) {

  path <- getOption("finbif_warehouse_query")

  if (count_only && identical(aggregate, "none")) {

    query[["selected"]] <- NULL
    query[["orderBy"]]  <- NULL
    path <- paste0(path, "unit/count")
    return(api_get(path, query, cache))

  }

  path <- paste0(path, select_endpoint(aggregate))

  if (count_only) {

    query[["page"]] <- 1L
    query[["pageSize"]] <- 1L
    resp <- api_get(path, query, cache)
    resp[["content"]] <- list(total = resp[["content"]][["total"]])
    return(resp)

  }

  query[["taxonCounts"]] <- taxa_counts(aggregate)

  query[["page"]] <- page
  query[["pageSize"]] <- min(n, max_size)

  resp <- list(records_obj(path, query, cache, select, aggregate))

  n_tot <- resp[[1L]][["content"]][["total"]]
  n <- min(n, n_tot)

  if (n > max_size) {

    # If random sampling and requesting few records or a large proportion of the
    # total number of records, it makes more sense to just get all the records
    # and sample afterwards to avoid coping with duplicates due to pagination.
    sample_after_request <- n_tot < max_size * 3L || n / n_tot > .5

    if (sample && sample_after_request) {
      all_records <- finbif_records(
        filter, select_, sample = FALSE, n = n_tot, quiet = quiet,
        cache = cache, dwc = dwc, df = df
      )
      return(record_sample(all_records, n, cache))
    }

    resp <- get_extra_pages(
      resp, n, max_size, quiet, path, query, cache, select, aggregate, df
    )

    if (sample) {
      resp <- handle_duplicates(
        resp, filter, select_, max_size, cache, n, seed = 1L, dwc, df
      )
    }

  }

  structure(
    resp, class = c("finbif_records_list", "finbif_api_list"), nrec_dnld = n,
    nrec_avl = n_tot, select = unique(select), select_user = unique(select_),
    record_id = record_id_selected, aggregate = aggregate, cache = cache
  )

}

# construct request ------------------------------------------------------------

records_obj <- function(path, query, cache, select, aggregate) {
  structure(
    api_get(path, query, cache),
    class = c("finbif_records", "finbif_api"),
    select = unique(select),
    aggregate = aggregate
  )
}

# record pagination ------------------------------------------------------------

get_extra_pages <- function(
  resp, n, max_size, quiet, path, query, cache, select, aggregate, df
) {

  multipage <- n > max_size

  if (multipage && !quiet) {
    pb_head("Fetching data")
    pb <- utils::txtProgressBar(0L, floor(n / max_size), style = 3L)
    on.exit(close(pb))
  }

  i <- 1L
  query[["page"]] <- query[["page"]] + 1L
  n_pages <- n %/% query[["pageSize"]]

  has_future <- requireNamespace("future", quietly = TRUE)

  if (has_future) value <- future::value

  while (multipage) {

    if (!quiet) utils::setTxtProgressBar(pb, i)

    if (query[["page"]] > n_pages) {

      excess_records <- n %% query[["pageSize"]]
      last_record <- query[["pageSize"]] * n_pages
      if (last_record == n) break
      query[["pageSize"]] <- get_next_lowest_factor(last_record, excess_records)
      query[["page"]] <- last_record / query[["pageSize"]] + 1L
      n_pages <- n %/% query[["pageSize"]]

    }

    delayedAssign("res", records_obj(path, query, cache, select, aggregate))

    if (has_future) {
      res <- future::future(records_obj(path, query, cache, select, aggregate))
    }

    if (df) attr(resp[[i]], "df") <- as.data.frame(resp[[i]])

    i <- i + 1L

    resp[[i]] <- value(res)

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

    if (filter_names[finbif_filter_names[[i]], "translated_values"]) {
      filter[[i]] <- translate(filter[[i]], names(filter)[[i]])
    }

    if (grepl("^(not_){0,1}collection$", names(filter)[[i]])) { # nolint

      if (inherits(filter[[i]], "finbif_collections")) {

        filter[[i]] <- row.names(filter[[i]])

      } else {

        env <- list()

        env[[names(filter)[[i]]]] <- finbif_collections(
          select = NA, supercollections = TRUE, nmin = NA
        )

        for (cl in c("id", "collection_name", "abbreviation")) {
          class(env[[names(filter)[[i]]]][[cl]]) <- "translation"
        }

        filter[[i]] <- translate(filter[[i]], names(filter)[[i]], env)

      }

    }

    if (identical(filter_names[finbif_filter_names[[i]], "class"], "coords")) {

      # Coordinates filter must have a system defined
      check_coordinates(finbif_filter_names[[i]], filter[["coordinates"]])

      filter[[i]] <- do.call(coords, as.list(filter[[i]]))

    }

    if (identical(filter_names[finbif_filter_names[[i]], "class"], "date")) {

      filter[[i]] <- do.call(
        dates, c(list(names(filter)[[i]]), as.list(filter[[i]]))
      )

    }

    filter[[i]] <- paste(
      filter[[i]], collapse = filter_names[finbif_filter_names[[i]], "sep"]
    )

  }

  names(filter) <- finbif_filter_names

  filter

}

check_coordinates <- function(filter_names, filter) {

  cond <- !is.null(names(filter)) && !identical(names(filter[[3]]), "")
  cond <- !utils::hasName(filter, "system") && cond
  cond <- length(filter) < 3L || cond
  cond <- identical(filter_names, "coordinates") && cond

  if (cond) deferrable_error("Invalid coordinates: system not specified")

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

    if (anyNA(ind)) {
      for (err in x[is.na(ind)]) {
        deferrable_error(
          paste0("Invalid name in ", gsub("_", " ", translation), ": ", err)
        )
      }
    }

    row.names(trsltn)[ind]

  }
}

# sample records ---------------------------------------------------------------

record_sample <- function(x, n, cache) {

  n_tot  <- attr(x, "nrec_dnld")
  select <- attr(x, "select")
  record_id <- attr(x, "record_id")

  if (cache) {
    records <- sample_with_seed(n_tot, n_tot - n, gen_seed(x))
  } else {
    records <- sample.int(n_tot, n_tot - n)
  }

  structure(
    remove_records(x, records),
    class = c(
      "finbif_records_sample_list", "finbif_records_list", "finbif_api_list"
    ),
    nrec_dnld = n,
    nrec_avl = n_tot,
    select = select,
    record_id = record_id,
    cache = cache
  )

}

# handle duplicates ------------------------------------------------------------

handle_duplicates <- function(
  x, filter, select, max_size, cache, n, seed, dwc, df
) {

  ids <- lapply(
    x,
    function(x) {
      vapply(
        x[["content"]][["results"]], get_el_recurse, NA_character_,
        c("unit", "unitId"), "character"
      )
    }
  )

  ids <- unlist(ids)

  duplicates <- which(duplicated(ids))

  x <- remove_records(x, duplicates)

  if (length(ids) - length(duplicates) < n) {

    new_records <- finbif_records(
      filter, select, sample = TRUE, n = max_size, cache = cache, dwc = dwc,
      seed = seed, df = df
    )

    x[[length(x) + 1L]] <- new_records[[1L]]

    x <- handle_duplicates(
      x, filter, select, max_size, cache, n, seed + 1L, dwc, df
    )

  }

  remove_records(x, n = n)

}

# remove records ---------------------------------------------------------------

remove_records <- function(x, records, n) {

  page_sizes <- vapply(x, function(x) x[["content"]][["pageSize"]], integer(1L))
  if (missing(records)) records <- seq(1L, sum(page_sizes))[-seq(1L, n)]
  excess_pages <- rep.int(seq_along(x), page_sizes)[records]
  records <- records - c(0L, cumsum(page_sizes)[-length(x)])[excess_pages]
  records <- split(records, excess_pages)

  for (i in seq_along(x)) {
    ind <- records[[as.character(i)]]
    x[[i]][["content"]][["results"]][ind] <- NULL
    if (!is.null(ind)) {
      attr(x[[i]], "df") <- attr(x[[i]], "df")[-ind, ]
    }
    new_page_size <- length(x[[i]][["content"]][["results"]])
    x[[i]][["content"]][["pageSize"]] <- new_page_size
  }

  x[!vapply(x, function(x) x[["content"]][["pageSize"]], integer(1L))] <- NULL

  x

}

# utils ------------------------------------------------------------------------

check_n <- function(n, nmax) {

  if (n > nmax) {
    deferrable_error(paste("Cannot download more than", nmax, "records"))
  }

  if (n < 1L) {
    deferrable_error(paste("Cannot request less than 1 record"))
  }

}

select_endpoint <- function(aggregate) {
  switch(
    aggregate[[1L]],
    none = "unit/list",
    events = "gathering/aggregate",
    "unit/aggregate"
  )
}

taxa_counts <- function(aggregate) {

  if (any(aggregate %in% c("species", "taxa"))) "true"

}
