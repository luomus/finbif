#' Download FinBIF occurrence records
#'
#' Download filtered occurrence data from FinBIF as a `data.frame`.
#'
#' @aliases fb_occurrence
#'
#' @param ... Character vectors or list of character vectors. Taxa of records
#'   to download.
#' @inheritParams finbif_records
#' @param date_time_method Character. Passed to `lutz::tz_lookup_coords()` when
#'   `date_time` and/or `duration` variables have been selected. Default is
#'   `"fast"` when  less than 100,000 records are requested and `"none"` when
#'   more. Using method `"none"` assumes all records are in timezone
#'    "Europe/Helsinki", Use `date_time_method = "accurate"` (requires package
#'     `sf`) for greater accuracy at the cost of slower computation.
#' @param check_taxa Logical. Check first that taxa are in the FinBIF database.
#'   If true only records that match known taxa (have a valid taxon ID) are
#'   returned.
#' @param on_check_fail Character. What to do if a taxon is found not valid. One
#'   of `"warn"` (default) or `"error"`.
#' @param tzone Character. If `date_time` has been selected the timezone of the
#'   outputted date-time. Defaults to system timezone.
#' @param drop_na Logical. A vector indicating which columns to check for
#'   missing data. Values recycled to the number of columns. Defaults to all
#'   columns.
#' @param aggregate_counts Logical. Should count variables be returned when
#'   using aggregation.
#' @param unlist Logical. Should variables that contain non atomic data be
#'  concatenated into a string separated by ";"?
#' @param facts Character vector. Extra variables to be extracted from record,
#'  event and document "facts".
#' @return A `data.frame`. If `count_only =  TRUE` an integer.
#' @examples \dontrun{
#'
#' # Get recent occurrence data for taxon
#' finbif_occurrence("Cygnus cygnus")
#'
#' # Specify the number of records
#' finbif_occurrence("Cygnus cygnus", n = 100)
#'
#' # Get multiple taxa
#' finbif_occurrence("Cygnus cygnus", "Ursus arctos")
#'
#' # Filter the records
#' finbif_occurrence(
#'   species = "Cygnus cygnus",
#'   filter = list(coordinate_accuracy_max = 100)
#' )
#'
#' }
#' @importFrom utils hasName
#' @importFrom lubridate as_datetime as.duration as.interval force_tzs
#' @importFrom lubridate format_ISO8601 hour interval minute ymd
#' @importFrom lutz tz_lookup_coords
#' @export

finbif_occurrence <- function(
  ..., filter = NULL, select = NULL, order_by = NULL, aggregate = "none",
  sample = FALSE, n = 10, page = 1, count_only = FALSE,
  quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"), dwc = FALSE, date_time_method = NULL,
  check_taxa = TRUE, on_check_fail = c("warn", "error"),
  tzone = getOption("finbif_tz"), locale = getOption("finbif_locale"),
  seed = NULL, drop_na = FALSE, aggregate_counts = TRUE, exclude_na = FALSE,
  unlist = FALSE, facts = NULL
) {

  fb_occurrence_obj <- list(
    taxa = c(...),
    filter = filter,
    select = select,
    order_by = order_by,
    aggregate = aggregate,
    sample = sample,
    n = n,
    page = page,
    count_only = count_only,
    quiet = quiet,
    cache = cache,
    dwc = dwc,
    date_time_method = date_time_method,
    check_taxa = check_taxa,
    on_check_fail = match.arg(on_check_fail),
    tzone = tzone,
    locale = locale,
    seed = seed,
    drop_na = drop_na,
    aggregate_counts = aggregate_counts,
    exclude_na = exclude_na,
    unlist = unlist,
    facts = facts
  )

  occurrence(fb_occurrence_obj)

}

occurrence <- function(fb_occurrence_obj) {

  taxa <- select_taxa(fb_occurrence_obj)

  fb_occurrence_obj[["taxa"]] <- taxa

  n <- fb_occurrence_obj[["n"]]

  date_time_method <- det_datetime_method(
    fb_occurrence_obj[["date_time_method"]], n = n
  )

  fb_occurrence_obj[["date_time_method"]] <- date_time_method

  filter <- fb_occurrence_obj[["filter"]]

  aggregate <- fb_occurrence_obj[["aggregate"]]

  select <- fb_occurrence_obj[["select"]]

  count_only <- fb_occurrence_obj[["count_only"]]

  quiet <- fb_occurrence_obj[["quiet"]]

  dwc <- fb_occurrence_obj[["dwc"]]

  locale <- fb_occurrence_obj[["locale"]]

  facts <- fb_occurrence_obj[["facts"]]

  if (!is.null(filter) && is.null(names(filter))) {

    stopifnot(
      "only one filter set can be used with aggregation" = identical(
        aggregate, "none"
      )
    )

    multi_request <- multi_req(fb_occurrence_obj)

    return(multi_request)

  }

  filter <- c(taxa, filter)

  include_facts <- !is.null(facts)

  fb_records_obj <- list(
    filter = filter,
    select = select,
    order_by = fb_occurrence_obj[["order_by"]],
    aggregate = aggregate,
    sample = fb_occurrence_obj[["sample"]],
    page = fb_occurrence_obj[["page"]],
    count_only = count_only,
    quiet = quiet,
    cache = fb_occurrence_obj[["cache"]],
    dwc = dwc,
    df = TRUE,
    seed = fb_occurrence_obj[["seed"]],
    exclude_na = fb_occurrence_obj[["exclude_na"]],
    locale = locale,
    include_facts = include_facts
  )

  if (!is.finite(n) || is.factor(n) || n < 0) {

    fb_records_obj[["n"]] <- getOption("finbif_max_page_size")

    n <- records(fb_records_obj)

    n <- attr(n, "nrec_avl")

    n <- pmax(n, getOption("finbif_max_page_size"))

  }

  fb_records_obj[["n"]] <- n

  records <- records(fb_records_obj)

  aggregate <- attr(records, "aggregate", TRUE)

  if (count_only) return(records[["content"]][["total"]])

  # Don't need a processing progress bar if only one page of records
  quiet <- quiet || length(records) < 2L

  pb_head("Processing data", quiet = quiet)

  df <- as.data.frame(records, locale = locale, quiet = quiet)

  n_col_nms <- grep("^n_", names(df), value = TRUE)

  ind <- !names(df) %in% n_col_nms

  names(df)[ind] <- var_names[names(df)[ind], col_type_string(dwc)]

  select_ <- attr(records, "select_user")

  select_ <- name_chr_vec(
    c(select_, n_col_nms[fb_occurrence_obj[["aggregate_counts"]]])
  )

  fb_occurrence_df <- structure(
    df,
    class = c("finbif_occ", "data.frame"),
    nrec_dnld = attr(records, "nrec_dnld", TRUE),
    nrec_avl  = attr(records, "nrec_avl", TRUE),
    select_user = select,
    column_names = select_,
    aggregate = aggregate,
    dwc = dwc,
    date_time = attr(records, "date_time", TRUE),
    date_time_method = date_time_method,
    tzone = fb_occurrence_obj[["tzone"]],
    locale = locale,
    include_new_cols = TRUE,
    facts = facts,
    unlist = fb_occurrence_obj[["unlist"]],
    drop_na = fb_occurrence_obj[["drop_na"]]
  )

  fb_occurrence_df <- date_times(fb_occurrence_df)

  fb_occurrence_df <- compute_date_time(fb_occurrence_df)

  fb_occurrence_df <- compute_duration(fb_occurrence_df)

  fb_occurrence_df <- compute_iso8601(fb_occurrence_df)

  fb_occurrence_df <- compute_vars_from_id(fb_occurrence_df)

  fb_occurrence_df <- compute_epsg(fb_occurrence_df)

  fb_occurrence_df <- compute_abundance(fb_occurrence_df)

  fb_occurrence_df <- compute_citation(fb_occurrence_df)

  fb_occurrence_df <- compute_coordinate_uncertainty(fb_occurrence_df)

  fb_occurrence_df <- compute_scientific_name(fb_occurrence_df)

  fb_occurrence_df <- compute_red_list_status(fb_occurrence_df)

  fb_occurrence_df <- compute_region(fb_occurrence_df)

  fb_occurrence_df <- extract_facts(fb_occurrence_df)

  select_ <- c(select_, name_chr_vec(as.character(facts)))

  fb_occurrence_df <- fb_occurrence_df[select_]

  attr(fb_occurrence_df, "select_user") <- select_

  fb_occurrence_df <- unlist_cols(fb_occurrence_df)

  fb_occurrence_df <- drop_na_col(fb_occurrence_df)

  names(fb_occurrence_df) <- names(select_)

  fb_occurrence_df

}

#' @noRd

select_taxa <- function(fb_occurrence_obj) {

  taxa <- fb_occurrence_obj[["taxa"]]

  cache <- fb_occurrence_obj[["cache"]]

  check_taxa <- fb_occurrence_obj[["check_taxa"]]

  on_check_fail <- fb_occurrence_obj[["on_check_fail"]]

  ntaxa <- length(taxa)

  if (identical(ntaxa, 0L)) return(NULL)

  ans <- list(taxon_name = paste(taxa, collapse = ","))

  if (check_taxa) {

    if (ntaxa > 1L || !utils::hasName(taxa, "taxa")) {

      taxa <- unlist(finbif_check_taxa(taxa, cache = cache))

    } else {

      taxa <- do.call(finbif_check_taxa, c(as.list(taxa), list(cache = cache)))

      taxa <- unlist(taxa)

    }

    taxa_invalid <- is.na(taxa)
    taxa_valid  <- !taxa_invalid

    if (any(taxa_invalid)) {
      msg  <- paste(
        "Cannot find the following taxa in the FinBIF taxonomy.",
        "Please check you are using accepted names and not synonyms or",
        "other names for the taxa you are selecting:\n",
        paste(sub("\\.", " - ", names(taxa[taxa_invalid])), collapse = ", "),
        sep = "\n"
      )
      switch(
        on_check_fail,
        warn  = warning(msg, call. = FALSE),
        error = stop(msg, call. = FALSE)
      )
    }

    if (any(taxa_valid)) {
      ans <- list(taxon_id = paste(taxa[taxa_valid], collapse = ","))
    }

  }

  ans

}

#' @noRd

date_times <- function(fb_occurrence_df) {

  df <- as.data.frame(fb_occurrence_df)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  date_start <- var_names["gathering.eventDate.begin", vtype]

  hour_start <- var_names["gathering.hourBegin", vtype]

  minute_start <- var_names["gathering.minutesBegin", vtype]

  date_end <- var_names["gathering.eventDate.end", vtype]

  hour_end <- var_names["gathering.hourEnd", vtype]

  minute_end <- var_names["gathering.minutesEnd", vtype]

  lat <- var_names["gathering.conversions.wgs84CenterPoint.lat", vtype]

  lon <- var_names["gathering.conversions.wgs84CenterPoint.lon", vtype]

  tzone <- attr(fb_occurrence_df, "tzone", TRUE)

  method <- attr(fb_occurrence_df, "date_time_method", TRUE)

  if (attr(fb_occurrence_df, "date_time", TRUE)) {

    date_time_start <- list(
      date = df[[date_start]],
      hour = df[[hour_start]],
      minute = df[[minute_start]],
      lat = df[[lat]],
      lon = df[[lon]],
      tzone = tzone,
      method = method
    )

    date_time_end <- list(
      date = df[[date_end]],
      hour = df[[hour_end]],
      minute = df[[minute_end]],
      lat = df[[lat]],
      lon = df[[lon]],
      tzone = tzone,
      method = method
    )

    fb_occurrence_df <- structure(
      fb_occurrence_df,
      date_time_start = date_time(date_time_start),
      date_time_end = date_time(date_time_end)
    )

  }

  fb_occurrence_df

}

#' @noRd

date_time <- function(date_time_obj) {

  tzone <- date_time_obj[["tzone"]]

  date_time <- as.POSIXct(character(), tz = tzone)

  if (length(date_time_obj[["date"]]) > 0L) {

    date_time <- lubridate::ymd(date_time_obj[["date"]])

    date_time <- lubridate::as_datetime(date_time)

    # When there is no hour assume the hour is midday (i.e., don't assume
    # midnight)
    lubridate::hour(date_time) <- 12L

    if (!is.null(date_time_obj[["hour"]])) {

      lubridate::hour(date_time) <- ifelse(
        is.na(date_time_obj[["hour"]]),
        lubridate::hour(date_time),
        date_time_obj[["hour"]]
      )

    }

    if (!is.null(date_time_obj[["minute"]])) {

      lubridate::minute(date_time) <- ifelse(
        is.na(date_time_obj[["minute"]]),
        lubridate::minute(date_time),
        date_time_obj[["minute"]]
      )

    }

    if (identical(date_time_obj[["method"]], "none")) {

      tz_in <- "Europe/Helsinki"

      date_time <- lubridate::force_tz(date_time, tz_in)
      date_time <- lubridate::with_tz(date_time, tzone)

    } else {

      tz_in <- lutz::tz_lookup_coords(
        date_time_obj[["lat"]],
        date_time_obj[["lon"]],
        date_time_obj[["method"]],
        FALSE
      )

      date_time <- lubridate::force_tzs(
        date_time,
        tzones = ifelse(is.na(tz_in), tzone, tz_in),
        tzone_out = tzone
      )

    }

    ind <- is.na(date_time_obj[["month"]]) | is.na(date_time_obj[["day"]])

    date_time[ind] <- lubridate::as_datetime(NA_integer_, tz = tzone)

  }

  date_time

}

#' @noRd

compute_date_time <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  date_time_var <- var_names["computed_var_date_time", vtype]

  column_names <- attr(fb_occurrence_df, "column_names", TRUE)

  if (date_time_var %in% column_names) {

    fb_occurrence_df[[date_time_var]] <- attr(
      fb_occurrence_df, "date_time_start", TRUE
    )

  }

  fb_occurrence_df

}

#' @noRd

compute_duration <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  duration_var <- var_names["computed_var_duration", vtype]

  column_names <- attr(fb_occurrence_df, "column_names", TRUE)

  if (duration_var %in% column_names) {

    hour_start <- var_names["gathering.hourBegin", vtype]

    hour_end <- var_names["gathering.hourEnd", vtype]

    date_time_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    date_time_end <- attr(fb_occurrence_df, "date_time_end", TRUE)

    ind <- is.na(fb_occurrence_df[[hour_start]])

    ind <- ind | is.na(fb_occurrence_df[[hour_end]])

    ind <- ind | is.na(date_time_start)

    ind <- ind | is.na(date_time_end)

    na_interval <- lubridate::as.interval(rep_len(NA_integer_, length(ind)))

    duration <- na_interval

    duration[!ind] <- lubridate::interval(
      date_time_start[!ind], date_time_end[!ind]
    )

    ind <- !is.na(duration) & duration == 0

    duration[ind] <- na_interval[ind]

    fb_occurrence_df[[duration_var]] <- lubridate::as.duration(duration)

  }

  fb_occurrence_df

}

#' @noRd

compute_iso8601 <- function(fb_occurrence_df) {

  df <- as.data.frame(fb_occurrence_df)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  date_start <- var_names["gathering.eventDate.begin", vtype]

  hour_start <- var_names["gathering.hourBegin", vtype]

  minute_start <- var_names["gathering.minutesBegin", vtype]

  date_end <- var_names["gathering.eventDate.end", vtype]

  hour_end <- var_names["gathering.hourEnd", vtype]

  minute_end <- var_names["gathering.minutesEnd", vtype]

  iso8601_var <- var_names["computed_var_date_time_ISO8601", vtype]

  column_names <- attr(fb_occurrence_df, "column_names", TRUE)

  if (iso8601_var %in% column_names) {

    tzone <- attr(fb_occurrence_df, "tzone", TRUE)

    date_time_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    date_time_end <- attr(fb_occurrence_df, "date_time_end", TRUE)

    ind <- is.na(date_time_start) | is.na(date_time_end)

    iso8601 <- lubridate::interval(
      rep_len("1970-01-01/1970-01-01", length(ind)), tzone = tzone
    )

    iso8601[!ind] <- lubridate::interval(
      date_time_start[!ind], date_time_end[!ind]
    )

    iso8601[ind] <- lubridate::as.interval(NA_integer_)

    iso8601 <- lubridate::format_ISO8601(iso8601, usetz = TRUE)

    no_start_time <- is.na(df[[minute_start]]) & is.na(df[[hour_start]])

    no_end_time <-  is.na(df[[minute_end]]) & is.na(df[[hour_end]])

    iso8601 <- ifelse(
      no_start_time | no_end_time,
      lubridate::format_ISO8601(
        lubridate::interval(
          lubridate::ymd(df[[date_start]]), lubridate::ymd(df[[date_end]])
        ),
        usetz = TRUE
      ),
      iso8601
    )

    no_duration <- date_time_start == date_time_end

    dates_equal <- df[[date_start]] == df[[date_end]]

    iso8601 <- ifelse(
      (no_end_time & (is.na(df[[date_end]]) | dates_equal)) | no_duration,
      lubridate::format_ISO8601(date_time_start, usetz = TRUE),
      iso8601
    )

    iso8601 <- ifelse(
      no_start_time & (is.na(df[[date_end]]) | dates_equal),
      lubridate::format_ISO8601(lubridate::ymd(df[[date_start]]), usetz = TRUE),
      iso8601
    )

    fb_occurrence_df[[iso8601_var]] <- ifelse(
      is.na(iso8601),
      ifelse(
        dates_equal,
        df[[date_start]],
        paste(df[[date_start]], df[[date_end]], sep = "/")
      ),
      iso8601
    )

  }

  fb_occurrence_df

}

#' @noRd

compute_vars_from_id <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  select_ <- attr(fb_occurrence_df, "column_names")

  dwc <- attr(fb_occurrence_df, "dwc")

  locale <- attr(fb_occurrence_df, "locale")

  add <- attr(fb_occurrence_df, "include_new_cols")

  candidates <- setdiff(select_, names(df))

  suffix <- switch(col_type_string(dwc), translated_var = "_id", dwc = "ID")

  for (k in seq_along(candidates)) {

    id_var_name <- paste0(candidates[[k]], suffix)

    if (utils::hasName(df, id_var_name) && add) {

      if (identical(id_var_name, "collection_id")) {

        ptrn <- "collection_name"

        metadata <- finbif_collections(
          select = ptrn, subcollections = TRUE,
          supercollections = TRUE, nmin = NA, locale = locale
        )

      } else {

        ptrn <- "^name_|^description_"

        metadata <- get(to_native(candidates[[k]]))

        if (!inherits(metadata, "data.frame")) {

          r <- unlist(lapply(metadata, row.names))

          metadata <- do.call(rbind, c(metadata, make.row.names = FALSE))

          row.names(metadata) <- r

        }

      }

      id_var <- df[[id_var_name]]

      j <- grep(ptrn, names(metadata))

      names(metadata) <- gsub(ptrn, "", names(metadata))

      i <- lapply(id_var, remove_domain)

      var <- lapply(i, function(i) metadata[i, j, drop = FALSE])

      var <- lapply(var, apply, 1L, as.list)

      var <- lapply(var, vapply, with_locale, NA_character_, locale)

      df[[candidates[[k]]]] <- mapply(
        function(x, y) unname(ifelse(is.na(x), y, x)),
        var, id_var,
        SIMPLIFY = FALSE, USE.NAMES = FALSE
      )

      if (!is.list(id_var)) {

        df[[candidates[[k]]]] <- unlist(df[[candidates[[k]]]])

      }

    }

  }

  df

}

#' @noRd

compute_epsg <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  select_ <- attr(fb_occurrence_df, "column_names")

  dwc <- attr(fb_occurrence_df, "dwc")

  add <- attr(fb_occurrence_df, "include_new_cols")

  select_ <- match(select_, var_names[, col_type_string(dwc)])

  select_ <- row.names(var_names[select_, ])

  crs <- c(computed_var_epsg = "", computed_var_fp_epsg = "footprint")

  for (i in seq_along(crs)[add]) {

    epsg <- c("euref", "ykj", "wgs84")

    names(epsg) <- epsg

    epsg[] <- paste0(crs[[i]], "_", epsg, "$")

    epsg <- lapply(epsg, grepl, var_names[select_, "translated_var"])

    epsg <- lapply(epsg, c, TRUE)

    epsg <- lapply(epsg, which)

    epsg <- vapply(epsg, min, integer(1L), USE.NAMES = TRUE)

    epsg <- names(which.min(epsg))

    epsg <- switch(
      epsg,
      euref = "EPSG:3067",
      ykj = "EPSG:2393",
      wgs84 = "EPSG:4326",
      NA_character_
    )

    epsg <- rep_len(epsg, nrow(df))

    df[[var_names[[names(crs)[[i]], col_type_string(dwc)]]]] <- epsg

  }

  df

}

#' @noRd

compute_abundance <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  select_ <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  locale <- attr(fb_occurrence_df, "locale", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  type <- col_type_string(dwc)

  abundance_ <- var_names[["computed_var_abundance", type]]

  occurrence_status_ <- var_names[["computed_var_occurrence_status", type]]

  abundance_i <- var_names[["unit.interpretations.individualCount", type]]

  abundance_v <- var_names[["unit.abundanceString", type]]

  if ((abundance_ %in% select_ || occurrence_status_ %in% select_) && add) {

    abundance <- ifelse(
      df[[abundance_i]] == 1L,
      ifelse(grepl("1", df[[abundance_v]]), 1L, NA_integer_),
      df[[abundance_i]]
    )

    if (abundance_ %in% select_) {

      df[[abundance_]] <- abundance

    }

    if (occurrence_status_ %in% select_) {

      status <- switch(
        locale,
        fi = c("paikalla", "poissa"),
        sv = c("n\u00e4rvarande", "fr\u00e5nvarande"),
        c("present", "absent")
      )

      occurrence_status <- ifelse(
        is.na(abundance) | abundance > 0L, status[[1L]], status[[2L]]
      )

      df[[occurrence_status_]] <- occurrence_status

    }

  }

  df

}

#' @noRd

compute_citation <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  select_ <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  record_id <- attr(fb_occurrence_df, "record_id", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  type <- col_type_string(dwc)

  citation <- var_names[["computed_var_citation", type]]

  source <- var_names[["document.sourceId", type]]

  document_id <- var_names[["document.documentId", type]]

  if (citation %in% select_ && add) {

    df[[citation]] <- ifelse(
      df[[source]] == "http://tun.fi/KE.3",
      df[[document_id]],
      record_id
    )

    df[[citation]] <- paste(df[[citation]], "Source: FinBIF")

  }

  df

}

#' @noRd

compute_coordinate_uncertainty <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  select_ <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  type <- col_type_string(dwc)

  coord_uncert_ <- var_names[["computed_var_coordinates_uncertainty", type]]

  coord_uncert_i <- var_names[[
    "gathering.interpretations.coordinateAccuracy", type
  ]]

  source <- var_names[["document.sourceId", type]]

  if (coord_uncert_ %in% select_ && add) {

    coord_uncert <- ifelse(
      df[[source]] == "http://tun.fi/KE.3" & df[[coord_uncert_i]] == 1,
      NA_real_,
      df[[coord_uncert_i]]
    )

    df[[coord_uncert_]] <- as.numeric(coord_uncert)

  }

  df

}

#' @noRd

compute_scientific_name <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  select_ <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  type <- col_type_string(dwc)

  scientific <- var_names[["computed_var_scientific_name", type]]

  scientific_ <- var_names[["unit.linkings.taxon.scientificName", type]]

  verbatim <- var_names[["unit.taxonVerbatim", type]]

  author <- var_names[["unit.linkings.taxon.scientificNameAuthorship", type]]

  verbatim_author <- var_names[["unit.author", type]]

  source <- var_names[["document.sourceId", type]]

  if (scientific %in% select_ && add) {

    df[[scientific]] <- ifelse(
      is.na(df[[scientific_]]) & df[[source]] == "http://tun.fi/KE.3",
      add_authors(
        list(names = df[[verbatim]], authors = df[[verbatim_author]])
      ),
      add_authors(
        list(names = df[[scientific_]], authors = df[[author]])
      )
    )

  }

  df

}

#' @noRd

add_authors <- function(names_obj) {

  names <- names_obj[["names"]]

  authors <- names_obj[["authors"]]

  authors <- ifelse(nchar(authors) > 1L, paste0(" ", authors), "")

  ifelse(is.na(names), names, paste0(names, authors))

}

#' @noRd

compute_red_list_status <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  select_ <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  type <- col_type_string(dwc)

  red_list_status <- var_names[["computed_var_red_list_status", type]]

  red_list_status_id <-
    var_names[["unit.linkings.taxon.latestRedListStatusFinland.status", type]]

  red_list_status_year <-
    var_names[["unit.linkings.taxon.latestRedListStatusFinland.year", type]]

  if (red_list_status %in% select_ && add) {

    df[[red_list_status]] <- ifelse(
      is.na(df[[red_list_status_id]]),
      NA_character_,
      paste(
        sub("http://tun.fi/MX.iucn", "", df[[red_list_status_id]]),
        df[[red_list_status_year]]
      )
    )

  }

  df

}

compute_region <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  select_ <- attr(fb_occurrence_df, "column_names", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  add <- attr(fb_occurrence_df, "include_new_cols", TRUE)

  type <- col_type_string(dwc)

  region <- var_names[["computed_var_region", type]]

  municipality_id <-
    var_names[["gathering.interpretations.finnishMunicipality", type]]

  if (region %in% select_ && add) {

    df[[region]] <- municipality[basename(df[[municipality_id]]), "region"]

  }

  df

}

#' @noRd

multi_req <- function(fb_occurrence_obj) {

  ans <- vector("list", length(fb_occurrence_obj[["filter"]]))

  rep_attr <- c(
    "sample", "n", "page", "quiet", "cache", "date_time_method", "tzone",
    "locale", "exclude_na"
  )

  for (attr in rep_attr) {

    fb_occurrence_obj[[attr]] <- rep_len(fb_occurrence_obj[[attr]], length(ans))

  }

  fb_occurrence_obj_i <- fb_occurrence_obj

  fb_occurrence_obj_i[["check_taxa"]] <- FALSE

  for (i in seq_along(ans)) {

    for (j in c("filter", rep_attr)) {

      fb_occurrence_obj_i[[j]] <- fb_occurrence_obj[[j]][[i]]

    }

    ans[[i]] <- occurrence(fb_occurrence_obj_i)

  }

  if (!fb_occurrence_obj[["count_only"]]) {

    ans <- do.call(rbind, ans)
    dups <- duplicated(attr(ans, "record_id"))
    ans <- ans[!dups, ]

  }

  ans

}

#' @noRd

unlist_cols <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  cols <- attr(fb_occurrence_df, "select_user", TRUE)

  unlist <- attr(fb_occurrence_df, "unlist", TRUE)

  if (unlist) {

    for (i in cols) {

      if (is.list(df[[i]]) && !grepl("Fact|fact_", i)) {

        df[[i]] <- vapply(df[[i]], concat_string, character(1L))

      }

    }

  }

  df

}

#' @noRd

extract_facts <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  facts <- attr(fb_occurrence_df, "facts", TRUE)

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  if (!is.null(facts)) {

    names <- c(
      "unit.facts.fact", "gathering.facts.fact", "document.facts.fact"
    )

    values <- c(
      "unit.facts.value", "gathering.facts.value", "document.facts.value"
    )

    df[facts] <- NA_character_

    for (fact in facts) {

      for (level in 1L:3L) {

        idx <- lapply(
          df[[var_names[[names[[level]], col_type_string(dwc)]]]], `==`, fact
        )

        vk <- mapply(
          function(v, k) ifelse(length(v[k]) > 0L, v[k], NA_character_),
          df[[var_names[[values[[level]], col_type_string(dwc)]]]],
          idx
        )

        if (!all(is.na(vk))) {

          df[[fact]] <- vk

        }

      }

    }

  }

  df

}

#' Get last modified date for FinBIF occurrence records
#'
#' Get last modified date for filtered occurrence data from FinBIF.
#'
#' @aliases fb_last_mod
#'
#' @inheritParams finbif_occurrence
#' @return A `Date` object
#' @examples \dontrun{
#'
#' # Get last modified date for Whooper Swan occurrence records from Finland
#' finbif_last_mod("Cygnus cygnus", filter = c(country = "Finland"))
#'
#' }
#' @export
finbif_last_mod <- function(..., filter) {

  res <- finbif_occurrence(
    ..., filter = filter, select = "load_date", order_by = "-load_date", n = 1L
  )

  ans <- as.Date(character())

  if (nrow(res) > 0L) {

    ans <- as.Date(res[["load_date"]][[1L]])

  }

  ans

}
