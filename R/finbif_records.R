#' @noRd
records <- function(fb_records_obj) {
  max_size <- getOption("finbif_max_page_size")
  fb_records_obj[["max_size"]] <- max_size
  max_queries <- getOption("finbif_max_queries")
  nmax <- max_queries * max_size
  fb_records_obj[["nmax"]] <- nmax
  var_type <- col_type_string(fb_records_obj[["dwc"]])
  fb_records_obj[["var_type"]] <- var_type
  query <- list()

  defer_errors({
    check_n(fb_records_obj)
    fb_records_obj[["n"]] <- as.integer(fb_records_obj[["n"]])
    aggregate <- infer_aggregation(fb_records_obj[["aggregate"]])
    fb_records_obj[["aggregate"]] <- aggregate

    if (!is.null(fb_records_obj[["filter"]])) {
      parsed_filters <- parse_filters(fb_records_obj)
      query <- lapply(parsed_filters, paste, collapse = ",")
    }

    select <- infer_selection(fb_records_obj)
    select_query <- select[["query"]]
    fb_records_obj[["select_query"]] <- select_query
    fb_records_obj[["select_user"]] <- select[["user"]]
    fb_records_obj[["record_id_selected"]] <- select[["record_id_selected"]]
    fb_records_obj[["date_time_selected"]] <- select[["date_time_selected"]]
    select_param <- switch(aggregate[[1L]], none = "selected", "aggregateBy")
    fb_records_obj[["select_param"]] <- select_param
    query[[select_param]] <- paste(select_query, collapse = ",")
    order_by <- fb_records_obj[["order_by"]]

    if (!is.null(order_by)) {
      desc_order <- grepl("^-", order_by)
      order_by <- sub("^-", "", order_by)
      var_names <- sysdata(list(which = "var_names"))
      var_names_order <- var_names[["order"]]
      order_vars <- var_names[var_names_order, var_type, drop = FALSE]
      order_vars[] <- lapply(order_vars, structure, class = "translation")
      order_vars <- list(order_vars = order_vars)
      order_vars <- list(
        x = order_by, translation = "order_vars", env = order_vars
      )
      order_by <- translate(order_vars)
      order_by <- order_by_computed_var(order_by)
      order_by[desc_order] <- paste(order_by[desc_order], "DESC")
      order_by <- paste(order_by, collapse = ",")
    }

    if (fb_records_obj[["sample"]]) {
      seed <- as.integer(fb_records_obj[["seed"]])
      seed <- c("RANDOM", seed)
      seed <- paste(seed, collapse = ":")
      order_by <- c(seed, order_by)
      order_by <- paste(order_by, collapse = ",")
    }

    query[["orderBy"]] <- order_by
  })

  fb_records_obj[["query"]] <- query
  fb_records_obj <- na_exclude(fb_records_obj)
  ans <- request(fb_records_obj)

  if (!fb_records_obj[["count_only"]]) {
    ind <- length(ans)
    last <- ans[[ind]]
    last[["locale"]] <- fb_records_obj[["locale"]]
    last[["cache"]] <- fb_records_obj[["cache"]]
    attr(last, "df") <- records_df(last)
    ans[[ind]] <- last
  }

  ans
}

#' @noRd
records_df <- function(x) {
  results <- x[[c("content", "results")]]
  cols <- attr(x, "select")
  var_names <- sysdata(list(which = "var_names"))
  single_col <- var_names[cols, "single"]
  aggregation <- attr(x, "aggregate")
  aggregated <- !identical(aggregation, "none")

  if (aggregated) {
    n_cols <- length(cols)
    single_col <- rep(TRUE, n_cols)
    aggregations <- c(
      records = "count",
      species = "speciesCount",
      taxa = "taxonCount",
      individuals = "individualCountSum",
      pairs = "pairCountSum",
      events = "count",
      documents = "count"
    )
    aggregations <- aggregations[aggregation]
    aggregation_nms <- names(aggregations)
    counts <- list()

    for (i in seq_along(aggregations)) {
      aggregation_nm <- aggregation_nms[[i]]
      count <- vapply(results, get_el_recurse, 0L, aggregations[[i]], "integer")
      na_count <- is.na(count)
      counts[[aggregation_nm]] <- ifelse(na_count, 0L, count)
    }

    results <- lapply(results, getElement, "aggregateBy")
  }

  attr(results, "select") <- cols
  attr(results, "locale") <- x[["locale"]]
  attr(results, "cache") <- x[["cache"]]
  attr(results, "aggregated") <- aggregated

  results <- process_cols(results)
  cols_split <- split(cols, single_col)
  cols_single <- cols_split[["TRUE"]]
  df <- as.data.frame(results[cols_single, drop = FALSE])

  cols_list <- cols_split[["FALSE"]]
  df[cols_list] <- results[cols_list, drop = FALSE]

  if (aggregated) {
    aggregation_cols <- paste0("n_", aggregation)
    cols <- c(cols, aggregation_cols)

    for (i in seq_along(aggregation)) {
      aggregation_col_i <- aggregation_cols[[i]]
      df[[aggregation_col_i]] <- counts[[i]]
    }

  }

  structure(
    df[cols],
    url = x[[c("response", "url")]],
    time = x[[c("response", "date")]]
  )
}

#' @noRd
process_cols <- function(x) {
  locale <- attr(x, "locale", TRUE)
  aggregated <- attr(x, "aggregated", TRUE)
  cache <- attr(x, "cache", TRUE)
  col_list <- list()
  var_names <- sysdata(list(which = "var_names"))

  for (col in attr(x, "select", TRUE)) {
    type  <- var_names[[col, "type"]]
    type_na <- cast_to_type(NA, type)
    single <- var_names[[col, "single"]]
    localised <- var_names[[col, "localised"]]
    labels_obj <- list(
      col = col, var_names = var_names, locale = locale, cache = cache
    )

    if (aggregated) {
      ans <- vapply(x, getElement, NA_character_, col)
      ans <- ifelse(ans == "", NA_character_, ans)

      if (localised) {
        labels_obj[["labels"]] <- ans
        ans <- localise_labels(labels_obj)
      }

      ans <- cast_to_type(ans, type)

    } else {
      col_els <- strsplit(col, "\\.")
      col_els <- col_els[[1L]]

      if (single) {
        ans <- vapply(x, get_el_recurse, type_na, col_els, type)

        if (localised) {
          labels_obj[["labels"]] <- ans
          ans <- localise_labels(labels_obj)
        }

      } else {
        ans <- lapply(x, get_el_recurse, col_els, type)
        ans <- lapply(ans, unlist)

        if (localised) {
          langs <- lapply(ans, names)
          langs <- unlist(langs)
          supported_langs <- sysdata(list(which = "supported_langs"))

          if (any(langs %in% supported_langs)) {
            ans <- vapply(ans, with_locale, type_na, locale)
          } else {
            for (i in seq_along(ans)) {
              labels_obj[["labels"]] <- ans[[i]]
              ans[[i]] <- localise_labels(labels_obj)
            }

          }
        }
      }
    }

    col_list[[col]] <- ans
  }

  col_list
}

#' @noRd
localise_labels <- function(labels_obj) {
  col <- labels_obj[["col"]]
  var_names <- labels_obj[["var_names"]]
  cache <- labels_obj[["cache"]]
  new_labels <- sysdata(
    list(
      which = var_names[[col, "translated_var"]],
      cache = cache[[2L]]
    )
  )
  locale_col <- paste0("name_", labels_obj[["locale"]])
  new_label_names <- names(new_labels)
  label_col <- which(new_label_names == locale_col)
  label_col <- max(1L, label_col)
  obj_labels <- labels_obj[["labels"]]
  labels_na <- is.na(obj_labels)
  localised_labels <- ifelse(
    labels_na, obj_labels, new_labels[obj_labels, label_col]
  )
  localised_labels_na <- is.na(localised_labels)
  ifelse(localised_labels_na, obj_labels, localised_labels)
}

# aggregation ------------------------------------------------------------------
#' @noRd
infer_aggregation <- function(aggregate) {
  single_agg <- c("none", "events", "documents")
  aggregations <- c(
    single_agg, "none", "records", "species", "taxa", "individuals", "pairs"
  )
  aggregate <- match.arg(aggregate, aggregations, TRUE)
  has_single_agg <- single_agg %in% aggregate

  if (length(aggregate) > 1L && any(has_single_agg)) {
    deferrable_error(
      "Chosen aggregation cannot by combined with other aggregations"
    )
  }

  aggregate
}

# selection --------------------------------------------------------------------
#' @noRd
infer_selection <- function(fb_records_obj) {
  aggregate <- fb_records_obj[["aggregate"]]
  aggregate <- aggregate[[1L]]
  select <- fb_records_obj[["select"]]
  var_type <- fb_records_obj[["var_type"]]
  var_names <- sysdata(list(which = "var_names"))
  is_date_time_vars <- var_names[["date"]]
  date_time_vars <- var_names[is_date_time_vars, ]
  is_default_vars <- var_names[["default_var"]]
  default_vars <- switch(
    aggregate,
    none = var_names[is_default_vars, ],
    events = var_names["gathering.gatheringId", ],
    documents = var_names["document.documentId", ],
    var_names["unit.linkings.taxon.scientificName", ]
  )
  aggregate_none <- identical(aggregate, "none")

  if (is.null(select)) {
    select <- row.names(default_vars)
    select_user <- default_vars[[var_type]]
    record_id_selected <- FALSE
    date_time_selected <- FALSE

    if (aggregate_none) {
      select <- c(
        select,
        row.names(date_time_vars),
        "unit.interpretations.individualCount",
        "unit.abundanceString",
        "gathering.interpretations.coordinateAccuracy",
        "unit.linkings.taxon.scientificName",
        "unit.taxonVerbatim",
        "unit.linkings.taxon.scientificNameAuthorship",
        "unit.author",
        "document.sourceId"
      )
      select <- unique(select)
      record_id_selected <- TRUE
      date_time_selected <- TRUE
    }

  } else {
    deselect <- grep("^-", select, value = TRUE)
    deselect <- substring(deselect, 2L)
    n_deselect <- length(deselect)
    n_select <- length(select)

    if (identical(n_deselect, n_select)) {
      select <- "default_vars"
    }

    select <- grep("^-", select, value = TRUE, invert = TRUE)
    default_vars <- list(default_vars[[var_type]])
    select <- ifelse(select == "default_vars", default_vars, select)
    select <- unlist(select)
    select <- select[!select %in% deselect]
    select_user <- select
    select <- unique(select)
    record_id <- var_names["unit.unitId", var_type]
    record_id_selected <- record_id %in% select

    if (!record_id_selected && aggregate_none) {
      select <- c(record_id, select)
    }

    date_time_var_translations <- c(
      "date_time",
      "eventDateTime",
      "date_time_ISO8601",
      "eventDate",
      "duration",
      "samplingEffort"
    )
    date_time_selected <- date_time_var_translations %in% select
    date_time_selected <- any(date_time_selected)

    if (date_time_selected) {
      select <- c(select, date_time_vars[[var_type]])
    }

    fb_records_obj[["select"]] <- select
    select <- infer_computed_vars(fb_records_obj)
    select_type <- switch(
      aggregate,
      none = "select",
      events = "aggregate_events",
      documents = "aggregate_documents",
      "aggregate"
    )
    select_type_rows <- var_names[[select_type]]

    select_vars <- var_names[select_type_rows, var_type, drop = FALSE]
    select_vars[] <- lapply(select_vars, structure, class = "translation")
    select_vars <- list(select_vars = select_vars)
    select_vars <- list(
      x = select, translation = "select_vars", env = select_vars
    )

    select <- translate(select_vars)
    vars_computed_from_id <- grepl("^computed_var_from_id", select)

    if (any(vars_computed_from_id)) {
      select_computed <- select[vars_computed_from_id]
      vars_computed_from_id <- var_names[select_computed, ]
      n_computed <- nrow(vars_computed_from_id)

      for (i in seq_len(n_computed)) {
        computed_names <- row.names(vars_computed_from_id)
        computed_var <- vars_computed_from_id[i, var_type]
        suffix <- switch(var_type, translated_var = "_id", dwc = "ID")
        select_vars[["x"]] <- paste0(computed_var, suffix)
        select[[match(computed_names[[i]], select)]] <- translate(select_vars)
      }

    }
  }

  if (fb_records_obj[["include_facts"]]) {
    select <- c(
      select,
      "unit.facts.fact",
      "unit.facts.value",
      "gathering.facts.fact",
      "gathering.facts.value",
      "document.facts.fact",
      "document.facts.value"
    )
  }

  uncomputed <- !grepl("^computed_var", select)
  list(
    query = unique(select[uncomputed]),
    user = select_user,
    record_id_selected = record_id_selected,
    date_time_selected = date_time_selected
  )
}

#' @noRd
infer_computed_vars <- function(fb_records_obj) {
  computable_vars <- list(
    abundance = list(
      vars = c(
        "computed_var_abundance", "computed_var_occurrence_status"
      ),
      select_names = c(
        "unit.interpretations.individualCount", "unit.abundanceString"
      )
    ),
    cu = list(
      vars = "computed_var_coordinates_uncertainty",
      select_names = c(
        "gathering.interpretations.coordinateAccuracy", "document.sourceId"
      )
    ),
    citation = list(
      vars = "computed_var_citation",
      select_names = c("document.documentId", "document.sourceId")
    ),
    sn = list(
      vars = "computed_var_scientific_name",
      select_names = c(
        "unit.linkings.taxon.scientificName",
        "unit.taxonVerbatim",
        "unit.linkings.taxon.scientificNameAuthorship",
        "unit.author",
        "document.sourceId"
      )
    ),
    red_list = list(
      vars = "computed_var_red_list_status",
      select_names = c(
        "unit.linkings.taxon.latestRedListStatusFinland.status",
        "unit.linkings.taxon.latestRedListStatusFinland.year"
      )
    ),
    region = list(
      vars = "computed_var_region",
      select_names = c(
        "gathering.interpretations.finnishMunicipality",
        "gathering.province"
      )
    ),
    institution_code = list(
      vars = "computed_var_institution_code",
      select_names = "document.collectionId"
    ),
    collection_code = list(
      vars = "computed_var_institution_code",
      select_names = "document.collectionId"
    ),
    country = list(
      vars = "computed_var_country",
      select_names = c("gathering.interpretations.country", "gathering.country")
    ),
    country_code = list(
      vars = "computed_var_country_code",
      select_names = c("gathering.interpretations.country", "gathering.country")
    ),
    municipality = list(
      vars = "computed_var_municipality",
      select_names = c(
        "gathering.interpretations.finnishMunicipality",
        "gathering.facts.fact",
        "gathering.facts.value"
      )
    ),
    local_area = list(
      vars = "computed_var_local_area",
      select_names = c(
        "gathering.interpretations.finnishMunicipality",
        "gathering.municipality"
      )
    )
  )

  select <- fb_records_obj[["select"]]
  var_names <- sysdata(list(which = "var_names"))
  var_type <- fb_records_obj[["var_type"]]

  for (i in computable_vars) {
    i_vars <- i[["vars"]]
    i_var_names <- var_names[i_vars, var_type]

    if (any(i_var_names %in% select)) {
      select_names_i <- i[["select_names"]]
      select <- c(select, var_names[select_names_i, var_type])
    }

  }

  select
}

# request ----------------------------------------------------------------------
#' @noRd
request <- function(fb_records_obj) {
  path <- getOption("finbif_warehouse_query")
  query <- fb_records_obj[["query"]]
  cache <- fb_records_obj[["cache"]]
  count_only <- fb_records_obj[["count_only"]]

  if (count_only && identical(fb_records_obj[["aggregate"]], "none")) {
    query[["selected"]] <- NULL
    query[["orderBy"]]  <- NULL
    request_obj <- list(
      path = paste0(path, "unit/count"),
      query = query,
      cache = cache[[1L]],
      restricted_api = fb_records_obj[["restricted_api"]]
    )
    return(api_get(request_obj))
  }

  aggregate <- fb_records_obj[["aggregate"]]
  endpoint <- switch(
    aggregate[[1L]],
    none = "unit/list",
    events = "gathering/aggregate",
    documents = "document/aggregate",
    "unit/aggregate"
  )
  path <- paste0(path, endpoint)
  fb_records_obj[["path"]] <- path

  if (count_only) {
    query[["page"]] <- 1L
    query[["pageSize"]] <- 1L
    request_obj <- list(
      path = path,
      query = query,
      cache = cache[[1L]],
      restricted_api = fb_records_obj[["restricted_api"]]
    )
    resp <- api_get(request_obj)
    resp[["content"]] <- list(total = resp[[c("content", "total")]])
    return(resp)
  }

  n <- fb_records_obj[["n"]]
  max_size <- fb_records_obj[["max_size"]]
  select_user <- fb_records_obj[["select_user"]]

  query[["taxonCounts"]] <- taxa_counts(fb_records_obj)
  query[["onlyCount"]] <- individual_counts(fb_records_obj)
  query[["pairCounts"]] <- pair_counts(fb_records_obj)
  query[["page"]] <- fb_records_obj[["page"]]
  query[["pageSize"]] <- min(n, max_size)
  fb_records_obj[["query"]] <- query

  resp <- api_get(fb_records_obj)
  resp <- structure(
    resp, select = fb_records_obj[["select_query"]], aggregate = aggregate
  )

  n_tot <- resp[[c("content", "total")]]
  n <- min(n, n_tot)

  fb_records_list <- structure(
    list(resp),
    max_size = max_size,
    quiet = fb_records_obj[["quiet"]],
    path = path,
    filter = fb_records_obj[["filter"]],
    query = query,
    nrec_dnld = n,
    nrec_avl = n_tot,
    sample = fb_records_obj[["sample"]],
    seed = fb_records_obj[["seed"]],
    select = fb_records_obj[["select_query"]],
    select_user = select_user,
    locale = fb_records_obj[["locale"]],
    dwc = fb_records_obj[["dwc"]],
    exclude_na = fb_records_obj[["exclude_na"]],
    include_facts = fb_records_obj[["include_facts"]],
    count_only = count_only,
    record_id = fb_records_obj[["record_id_selected"]],
    date_time = fb_records_obj[["date_time_selected"]],
    aggregate = aggregate,
    cache = cache,
    restricted_api = fb_records_obj[["restricted_api"]],
    from_cache = resp[["from_cache"]]
  )

  get_extra_pages(fb_records_list)
}

# record pagination ------------------------------------------------------------
#' @noRd
#' @importFrom utils txtProgressBar setTxtProgressBar
get_extra_pages <- function(fb_records_list) {
  fb_records_obj <- list(
    path = attr(fb_records_list, "path", TRUE),
    cache = attr(fb_records_list, "cache", TRUE),
    select_query = attr(fb_records_list, "select", TRUE),
    aggregate = attr(fb_records_list, "aggregate", TRUE),
    restricted_api = attr(fb_records_list, "restricted_api", TRUE),
    cache_override = attr(fb_records_list, "from_cache", TRUE)
  )

  n <- attr(fb_records_list, "nrec_dnld", TRUE)
  n_tot <- attr(fb_records_list, "nrec_avl", TRUE)
  max_size <- attr(fb_records_list, "max_size", TRUE)
  multipage <- n > max_size
  quiet <- attr(fb_records_list, "quiet", TRUE)
  use_progress <- multipage && !quiet

  i <- 1L

  if (use_progress) {
    max <- floor(n / max_size)
    pb <- utils::txtProgressBar(0L, max, style = 3L)
    utils::setTxtProgressBar(pb, i)
    on.exit(close(pb))
  }

  query <- attr(fb_records_list, "query", TRUE)
  sample <- attr(fb_records_list, "sample", TRUE)
  page <- query[["page"]]
  last_page <- ceiling(n_tot / query[["pageSize"]])

  use_future <- has_pkgs("future") && getOption("finbif_use_async")

  if (use_future) {
    value <- future::value
  }

  n_needed <- n
  n_got <- query[["pageSize"]]
  ids <- character()
  more_pages <- TRUE

  while (n_needed > n_got - length(which(duplicated(ids))) && more_pages) {

    page <- page + 1L
    sampling_failed <- sample && page > last_page

    if (sampling_failed) {
      sample <- FALSE
      page <- 1L
      query[["orderBy"]] <- sub(
        ",?RANDOM:?\\d*|^RANDOM:?\\d*,?", "", query[["orderBy"]]
      )
      use_progress <- FALSE
    }

    query[["page"]] <- page
    fb_records_obj[["query"]] <- query
    delayedAssign("res", api_get(fb_records_obj))

    if (use_future) {
      res <- future::future(api_get(fb_records_obj), seed = NULL)
    }

    attr(fb_records_list[[i]], "df") <- records_df(fb_records_list[[i]])

    records_i <- value(res)
    records_i <- c(records_i, locale = fb_records_list[[i]][["locale"]])

    i <- i + 1L

    if (use_progress) {
      utils::setTxtProgressBar(pb, i)
    }

    fb_records_list[[i]] <- structure(
      records_i,
      select = fb_records_obj[["select_query"]],
      aggregate = fb_records_obj[["aggregate"]]
    )

    ids <- lapply(fb_records_list, extract_ids)
    ids <- unlist(ids)
    ids <- ids[!is.na(ids)]

    n_got <- n_got + length(records_i[[c("content", "results")]])
    more_pages <- page < last_page || sample
  }

  fb_records_list
}

# parsing filters --------------------------------------------------------------
#' @noRd
parse_filters <- function(fb_records_obj) {
  filter <- as.list(fb_records_obj[["filter"]])
  cache <- fb_records_obj[["cache"]]

  finbif_filter_names <- list(x = names(filter), translation = "filter_names")
  finbif_filter_names <- translate(finbif_filter_names)

  has_taxon <- any(finbif_filter_names %in% c("taxonId", "target"))
  has_events_or_docs <- any(
    fb_records_obj[["aggregate"]] %in% c("events", "documents")
  )

  if (has_taxon && has_events_or_docs) {
    deferrable_error("Cannot use current aggregation and filter by taxon")
  }

  filter_names <- sysdata(list(which = "filter_names"))

  for (i in seq_along(filter)) {
    filter_name_i <- finbif_filter_names[[i]]

    if (is.na(filter_name_i)) {
      next
    }

    nms <- names(filter)
    nm_i <- nms[[i]]
    f_i <- filter[[i]]

    if (filter_names[[filter_name_i, "translated_values"]]) {
      f_i <- list(x = f_i, translation = nm_i, cache = cache)
      f_i <- translate(f_i)
    }

    class <- filter_names[[filter_name_i, "class"]]

    if (grepl("^(not_){0,1}collection(_exclusive){0,1}$", nm_i)) {
      if (inherits(f_i, "finbif_collections")) {
        f_i <- row.names(f_i)
      } else {
        collections <- finbif_collections(
          select = c("id", "collection_name", "abbreviation"),
          supercollections = TRUE,
          nmin = NA,
          locale = fb_records_obj[["locale"]]
        )
        collections[] <- lapply(collections, structure, class = "translation")

        env <- list()
        env[[nm_i]] <- collections

        f_i <- remove_domain(f_i)
        f_i <- list(x = f_i, translation = nm_i, env = env, cache = cache)
        f_i <- translate(f_i)
      }
    } else if (identical(class, "coords")) {
      coordinates_filter <- filter[["coordinates"]]
      coordinates_obj <- list(name = filter_name_i, filter = coordinates_filter)
      check_coordinates(coordinates_obj)

      f_i <- coords(f_i)
    } else if (identical(class, "date")) {
      date_filter <- list(filter = nm_i)
      date_filter <- switch(
        class(f_i), Interval = list(filter = nm_i, f_i), c(date_filter, f_i)
      )

      f_i <- dates(date_filter)
    }

    filter[[i]] <- paste(f_i, collapse = filter_names[filter_name_i, "sep"])
  }

  names(filter) <- finbif_filter_names
  filter
}

#' @noRd
check_coordinates <- function(obj) {
  filter <- obj[["filter"]]
  nms <- names(filter)
  no_system <- !is.null(nms) && !identical(nms[[3L]], "") && !"system" %in% nms

  if (no_system || length(filter) < 3L) {
    deferrable_error("Invalid coordinates: system not specified")
  }
}

# translation ------------------------------------------------------------------
#' @noRd
translate <- function(translation_obj) {
  x <- translation_obj[["x"]]
  translation <- translation_obj[["translation"]]
  lst <- translation_obj[["env"]]
  cache <- translation_obj[["cache"]]

  if (translation %in% names(lst)) {
    trsltn <- lst[[translation]]
  } else {
    trsltn <- sysdata(list(which = translation, cache = cache[[2L]]))
  }

  if (is.list(x)) {
    nms <- names(x)
    translation_names <- names(trsltn)
    translation_names1 <- translation_names[[1L]]

    nms <- list(
      x = nms,
      translation = translation_names1,
      env = trsltn,
      cache = cache
    )
    nms <- translate(nms)
    names(x) <- nms

    translation_names2 <- translation_names[[2L]]
    x <- lapply(x, list, translation_names2, trsltn)
    translation_obj_names <- c("x", "translation", "env")
    x <- lapply(x, structure, names = translation_obj_names)
    x <- lapply(x, translate)
    x <- lapply(x, paste, collapse = ",")
    nms <- names(x)
    unempty_x <- sprintf("[%s]", x)
    x <- ifelse(x == "", x, unempty_x)
    ans <- sprintf("%s%s", nms, x)
  } else {
    n <- length(x)
    ind <- rep(NA_integer_, n)

    if (!is.data.frame(trsltn)) {
      trsltn <- trsltn[[1L]]
    }

    x_low <- tolower(x)

    for (i in trsltn) {
      if (inherits(i, "translation")) {
        i_low <- tolower(i)
        matched <- match(x_low, i_low)
        matched_na <- is.na(matched)
        ind <- ifelse(matched_na, ind, matched)
      }
    }

    if (anyNA(ind)) {
      for (err in x[is.na(ind)]) {
        translation <- gsub("_", " ", translation)
        deferrable_error(paste0("Invalid name in ", translation, ": ", err))
      }
    }

    ans <- row.names(trsltn)
    ans <- ans[ind]
    ans <- ans[!grepl("DUPLICATE", ans)]
  }

  ans
}

# utils ------------------------------------------------------------------------
#' @noRd
extract_ids <- function(x) {
  vapply(
    x[[c("content", "results")]],
    get_el_recurse,
    "",
    c("unit", "unitId"),
    "character"
  )
}

#' @noRd
check_n <- function(fb_records_obj) {
  n <- fb_records_obj[["n"]]
  nmax <- fb_records_obj[["nmax"]]

  if (as.numeric(n) > nmax) {
    msg <- paste("Cannot download more than", nmax, "records")
    deferrable_error(msg)
  }

  if (n < 1L) {
    deferrable_error("Cannot request less than 1 record")
  }

}

#' @noRd
taxa_counts <- function(fb_records_obj) {
  ans <- NULL

  if (any(c("species", "taxa") %in% fb_records_obj[["aggregate"]])) {
    ans <- "true"
  }

  ans

}

#' @noRd
individual_counts <- function(fb_records_obj) {
  ans <- "false"

  if (!"individuals" %in% fb_records_obj[["aggregate"]]) {
    ans <- NULL
  }

  ans

}

#' @noRd
pair_counts <- function(fb_records_obj) {
  ans <- "true"

  if (!"pairs" %in% fb_records_obj[["aggregate"]]) {
    ans <- NULL
  }

  ans

}

#' @noRd
order_by_computed_var <- function(order_by) {
  lt <- c(
    scientific_name = "unit.linkings.taxon.scientificName",
    abundance = "unit.interpretations.individualCount",
    date_time = "gathering.eventDate.begin",
    coordinates_uncertainty = "gathering.interpretations.coordinateAccuracy",
    red_list_status = "unit.linkings.taxon.redListStatus",
    citation = "document.documentId",
    occurrence_status = "unit.interpretations.individualCount",
    duration = "gathering.eventDate.begin",
    region = "gathering.province",
    date_time_ISO8601 = "gathering.eventDate.begin",
    municipality = "gathering.interpretations.finnishMunicipality",
    local_area = "gathering.interpretations.finnishMunicipality",
    collection_code = "document.collectionId",
    institution_code = "document.collectionId",
    country = "gathering.interpretations.country",
    country_code = "gathering.interpretations.country"
  )

  nms <- names(lt)
  names(lt) <- paste0("computed_var_", nms)

  ans <- lt[order_by]
  is_na <- is.na(ans)
  ans <- ifelse(is_na, order_by, ans)
  as.character(ans)
}

#' @noRd
na_exclude <- function(fb_records_obj) {
  query <- fb_records_obj[["query"]]

  if (fb_records_obj[["exclude_na"]]) {
    select_param <- fb_records_obj[["select_param"]]
    has_value <- strsplit(query[[select_param]], ",")
    has_value <- c(hasValue = has_value)

    v <- sysdata(list(which = "var_names"))
    available_vars <- row.names(v)

    i <- v[["aggregate"]] | v[["aggregate_events"]] | v[["aggregate_documents"]]
    i <- i & v[["single"]]

    has_value <- lapply(has_value, intersect, available_vars[i])
    has_value <- lapply(has_value, paste, collapse = ",")

    query <- c(query, has_value)
  }

  fb_records_obj[["query"]] <- query
  fb_records_obj
}
