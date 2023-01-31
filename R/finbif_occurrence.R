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
#' @importFrom lubridate as_datetime as.duration as.interval force_tzs
#' @importFrom lubridate format_ISO8601 hour interval minute ymd
#' @importFrom lutz tz_lookup_coords
#' @export

finbif_occurrence <- function(
  ...,
  filter = NULL,
  select = NULL,
  order_by = NULL,
  aggregate = "none",
  sample = FALSE,
  n = 10,
  page = 1,
  count_only = FALSE,
  quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"),
  dwc = FALSE,
  date_time_method = NULL,
  check_taxa = TRUE,
  on_check_fail = c("warn", "error"),
  tzone = getOption("finbif_tz"),
  locale = getOption("finbif_locale"),
  seed = NULL,
  drop_na = FALSE,
  aggregate_counts = TRUE,
  exclude_na = FALSE,
  unlist = FALSE,
  facts = NULL
) {

  taxa <- c(...)

  on_check_fail <- match.arg(on_check_fail)

  fb_occurrence_obj <- list(
    taxa = taxa,
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
    on_check_fail = on_check_fail,
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

#' @noRd

occurrence <- function(fb_occurrence_obj) {

  taxa <- select_taxa(fb_occurrence_obj)

  fb_occurrence_obj[["taxa"]] <- taxa

  n <- fb_occurrence_obj[["n"]]

  fb_occurrence_obj <- det_datetime_method(fb_occurrence_obj)

  date_time_method <- fb_occurrence_obj[["date_time_method"]]

  filter <- fb_occurrence_obj[["filter"]]

  aggregate <- fb_occurrence_obj[["aggregate"]]

  select <- fb_occurrence_obj[["select"]]

  count_only <- fb_occurrence_obj[["count_only"]]

  quiet <- fb_occurrence_obj[["quiet"]]

  dwc <- fb_occurrence_obj[["dwc"]]

  locale <- fb_occurrence_obj[["locale"]]

  facts <- fb_occurrence_obj[["facts"]]

  filter_names <- names(filter)

  no_filter_names <- is.null(filter_names)

  multi_filter <- no_filter_names && !is.null(filter)

  if (multi_filter) {

    aggregate_none <- identical(aggregate, "none")

    stopifnot(
      "Only one filter set can be used with aggregation" = aggregate_none
    )

    multi_request <- multi_req(fb_occurrence_obj)

    return(multi_request)

  }

  filter <- c(taxa, filter)

  include_facts <- !is.null(facts)

  order_by <- fb_occurrence_obj[["order_by"]]

  sample <- fb_occurrence_obj[["sample"]]

  page <- fb_occurrence_obj[["page"]]

  cache <- fb_occurrence_obj[["cache"]]

  seed <- fb_occurrence_obj[["seed"]]

  exclude_na <- fb_occurrence_obj[["exclude_na"]]

  fb_records_obj <- list(
    filter = filter,
    select = select,
    order_by = order_by,
    aggregate = aggregate,
    sample = sample,
    page = page,
    count_only = count_only,
    quiet = quiet,
    cache = cache,
    dwc = dwc,
    df = TRUE,
    seed = seed,
    exclude_na = exclude_na,
    locale = locale,
    include_facts = include_facts
  )

  needs_n <- n < 0

  needs_n <- needs_n || is.factor(n)

  needs_n <- needs_n || !is.finite(n)

  if (needs_n) {

    max_page_size <- getOption("finbif_max_page_size")

    fb_records_obj[["n"]] <- max_page_size

    n <- records(fb_records_obj)

    n <- attr(n, "nrec_avl")

    n <- pmax(n, max_page_size)

  }

  fb_records_obj[["n"]] <- n

  records <- records(fb_records_obj)

  aggregate <- attr(records, "aggregate", TRUE)

  if (count_only) {

    ans <- records[["content"]]

    ans <- ans[["total"]]

    return(ans)

  }

  # Don't need a processing progress bar if only one page of records

  quiet <- quiet || length(records) < 2L

  pb_head("Processing data", quiet = quiet)

  df <- as.data.frame(records, locale = locale, quiet = quiet)

  colnames <- names(df)

  n_col_nms <- grep("^n_", colnames, value = TRUE)

  ind <- !colnames %in% n_col_nms

  vtype <- col_type_string(dwc)

  non_count_cols <- colnames[ind]

  non_count_cols <- var_names[non_count_cols, vtype]

  colnames[ind] <- non_count_cols

  names(df) <- colnames

  select_user <- attr(records, "select_user")

  aggregate_counts <- fb_occurrence_obj[["aggregate_counts"]]

  n_col_nms <- n_col_nms[aggregate_counts]

  select_user <- c(select_user, n_col_nms)

  select_user <- name_chr_vec(select_user)

  class <- c("finbif_occ", "data.frame")

  nrec_dnld <- attr(records, "nrec_dnld", TRUE)

  nrec_avl <- attr(records, "nrec_avl", TRUE)

  date_time <- attr(records, "date_time", TRUE)

  tzone <- fb_occurrence_obj[["tzone"]]

  unlist <- fb_occurrence_obj[["unlist"]]

  drop_na <- fb_occurrence_obj[["drop_na"]]

  fb_occurrence_df <- structure(
    df,
    class = class,
    nrec_dnld = nrec_dnld,
    nrec_avl  = nrec_avl,
    select_user = select,
    column_names = select_user,
    aggregate = aggregate,
    dwc = dwc,
    date_time = date_time,
    date_time_method = date_time_method,
    tzone = tzone,
    locale = locale,
    include_new_cols = TRUE,
    facts = facts,
    unlist = unlist,
    drop_na = drop_na
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

  facts <- as.character(facts)

  facts <- name_chr_vec(facts)

  select_user <- c(select_user, facts)

  fb_occurrence_df <- fb_occurrence_df[select_user]

  attr(fb_occurrence_df, "select_user") <- select_user

  fb_occurrence_df <- unlist_cols(fb_occurrence_df)

  fb_occurrence_df <- drop_na_col(fb_occurrence_df)

  names(fb_occurrence_df) <- names(select_user)

  fb_occurrence_df

}

#' @noRd

det_datetime_method <- function(obj) {

  n <- obj[["n"]]

  method <- obj[["date_time_method"]]

  is_null <- is.null(method)

  if (is_null) {

    method <- "none"

    is_num <- is.numeric(n)

    is_pos <- n >= 0L

    cond <- is_num & is_pos

    n <- ifelse(cond, n, Inf)

    n <- sum(n)

    n_small <- n < 1e5L

    if (n_small) {

      method <- "fast"

    }

  }

  obj[["date_time_method"]] <- method

  obj

}

#' @noRd

select_taxa <- function(fb_occurrence_obj) {

  taxa <- fb_occurrence_obj[["taxa"]]

  cache <- fb_occurrence_obj[["cache"]]

  check_taxa <- fb_occurrence_obj[["check_taxa"]]

  on_check_fail <- fb_occurrence_obj[["on_check_fail"]]

  ntaxa <- length(taxa)

  no_taxa <- identical(ntaxa, 0L)

  if (no_taxa) {

    return(NULL)

  }

  taxon_name <- paste(taxa, collapse = ",")

  ans <- list(taxon_name = taxon_name)

  if (check_taxa) {

    taxa_names <- names(taxa)

    no_taxa_name <- !"taxa" %in% taxa_names

    cond <- no_taxa_name || ntaxa > 1L

    if (cond) {

      taxa <- finbif_check_taxa(taxa, cache = cache)

    } else {

      cache <- list(cache = cache)

      taxa <- as.list(taxa)

      taxa <- c(taxa, cache)

      taxa <- do.call(finbif_check_taxa, taxa)

    }

    taxa <- unlist(taxa)

    taxa_invalid <- is.na(taxa)

    any_taxa_invalid <- any(taxa_invalid)

    if (any_taxa_invalid) {

      invalid_taxa <- taxa[taxa_invalid]

      invalid_taxa_names <- names(invalid_taxa)

      msg <- sub("\\.", " - ", invalid_taxa_names)

      msg <- paste(msg, collapse = ", ")

      msg <- paste(
        "Cannot find the following taxa in the FinBIF taxonomy.",
        "Please check you are using accepted names and not synonyms or",
        "other names for the taxa you are selecting:\n",
        msg,
        sep = "\n"
      )

      switch(
        on_check_fail,
        warn  = warning(msg, call. = FALSE),
        error = stop(msg, call. = FALSE)
      )

    }

    taxa_valid <- !taxa_invalid

    any_taxa_valid <- any(taxa_valid)

    if (any_taxa_valid) {

      valid_taxa <- taxa[taxa_valid]

      valid_taxa <- paste(valid_taxa, collapse = ",")

      ans <- list(taxon_id = valid_taxa)

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

  date_time <- attr(fb_occurrence_df, "date_time", TRUE)

  if (date_time) {

    date_start <- df[[date_start]]

    hour_start <-  df[[hour_start]]

    minute_start <-  df[[minute_start]]

    date_end <- df[[date_end]]

    hour_end <-  df[[hour_end]]

    minute_end <-  df[[minute_end]]

    lat <- df[[lat]]

    lon <- df[[lon]]

    date_time_start <- list(
      date = date_start,
      hour = hour_start,
      minute = minute_start,
      lat = lat,
      lon = lon,
      tzone = tzone,
      method = method
    )

    date_time_end <- list(
      date = date_end,
      hour = hour_end,
      minute = minute_end,
      lat = lat,
      lon = lon,
      tzone = tzone,
      method = method
    )

    date_time_start <- date_time(date_time_start)

    date_time_end <- date_time(date_time_end)

    fb_occurrence_df <- structure(
      fb_occurrence_df,
      date_time_start = date_time_start,
      date_time_end = date_time_end
    )

  }

  fb_occurrence_df

}

#' @noRd
#' @importFrom lubridate as_datetime force_tz hour minute with_tz ymd
#' @importFrom lutz tz_lookup_coords

date_time <- function(date_time_obj) {

  tzone <- date_time_obj[["tzone"]]

  date_time <- character()

  date_time <- as.POSIXct(date_time, tz = tzone)

  date <- date_time_obj[["date"]]

  date_length <- length(date)

  date_has_length <- date_length > 0L

  if (date_has_length) {

    date_time <- lubridate::ymd(date)

    date_time <- lubridate::as_datetime(date_time)

    # When there is no hour assume the hour is midday (i.e., don't assume
    # midnight)

    hour <- date_time_obj[["hour"]]

    has_hour <- !is.null(hour)

    if (has_hour) {

      hour_is_na <- is.na(hour)

      date_time_hour <- ifelse(hour_is_na, 12L, hour)

    }

    lubridate::hour(date_time) <- date_time_hour

    minute <- date_time_obj[["minute"]]

    has_minute <- !is.null(minute)

    if (has_minute) {

      date_time_minute <- lubridate::minute(date_time)

      minute_is_na <- is.na(minute)

      date_time_minute <- ifelse(minute_is_na, date_time_minute, minute)

      lubridate::minute(date_time) <- date_time_minute

    }

    method <- date_time_obj[["method"]]

    no_method <- identical(method, "none")

    if (no_method) {

      tz_in <- "Europe/Helsinki"

      date_time <- lubridate::force_tz(date_time, tz_in)

      date_time <- lubridate::with_tz(date_time, tzone)

    } else {

      lat <- date_time_obj[["lat"]]

      lon <- date_time_obj[["lon"]]

      tz_in <- lutz::tz_lookup_coords(lat, lon, method, FALSE)

      tz_in_is_na <- is.na(tz_in)

      tzones <- ifelse(tz_in_is_na, tzone, tz_in)

      date_time <- lubridate::force_tzs(date_time, tzones, tzone)

    }

    month <- date_time_obj[["month"]]

    is_na_month <- is.na(month)

    day <- date_time_obj[["day"]]

    is_na_day <- is.na(day)

    is_na_month_day <- is_na_month | is_na_day

    na_month_day <- lubridate::as_datetime(NA_integer_, tz = tzone)

    date_time[is_na_month_day] <- na_month_day

  }

  date_time

}

#' @noRd

compute_date_time <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  date_time_var <- var_names["computed_var_date_time", vtype]

  column_names <- attr(fb_occurrence_df, "column_names", TRUE)

  has_date_time <- date_time_var %in% column_names

  if (has_date_time) {

    date_time_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    fb_occurrence_df[[date_time_var]] <- date_time_start

  }

  fb_occurrence_df

}

#' @noRd
#' @importFrom lubridate as.interval as.duration interval

compute_duration <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  duration_var <- var_names["computed_var_duration", vtype]

  column_names <- attr(fb_occurrence_df, "column_names", TRUE)

  has_duration <- duration_var %in% column_names

  if (has_duration) {

    hour_start <- var_names["gathering.hourBegin", vtype]

    hour_end <- var_names["gathering.hourEnd", vtype]

    date_time_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    date_time_end <- attr(fb_occurrence_df, "date_time_end", TRUE)

    hour_start <- fb_occurrence_df[[hour_start]]

    hour_start_is_na <- is.na(hour_start)

    hour_end <- fb_occurrence_df[[hour_end]]

    hour_end_is_na <- is.na(hour_end)

    hour_is_na <- hour_start_is_na | hour_end_is_na

    date_time_start_is_na <- is.na(date_time_start)

    date_time_end_is_na <- is.na(date_time_end)

    date_time_is_na <- date_time_start_is_na | date_time_end_is_na

    duration_is_na <- hour_is_na | date_time_is_na

    duration_length <- length(duration_is_na)

    interval <- rep_len(NA_integer_, duration_length)

    interval <- lubridate::as.interval(interval)

    date_time_start <- date_time_start[!duration_is_na]

    date_time_end <- date_time_end[!duration_is_na]

    date_time_interval <- lubridate::interval(date_time_start, date_time_end)

    duration <- interval

    interval[!duration_is_na] <- date_time_interval

    zero_interval <- interval == 0

    has_duration <- !duration_is_na & !zero_interval

    date_time_interval <- interval[has_duration]

    duration[has_duration] <- date_time_interval

    duration <- lubridate::as.duration(duration)

    fb_occurrence_df[[duration_var]] <- duration

  }

  fb_occurrence_df

}

#' @noRd
#' @importFrom lubridate as.interval format_ISO8601 interval ymd

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

  has_iso8601 <- iso8601_var %in% column_names

  if (has_iso8601) {

    tzone <- attr(fb_occurrence_df, "tzone", TRUE)

    date_time_start <- attr(fb_occurrence_df, "date_time_start", TRUE)

    date_time_end <- attr(fb_occurrence_df, "date_time_end", TRUE)

    date_time_start_is_na <- is.na(date_time_start)

    date_time_end_is_na <- is.na(date_time_end)

    duration_is_na <- date_time_start_is_na | date_time_end_is_na

    duration_length <- length(duration_is_na)

    iso8601 <- rep_len("1970-01-01/1970-01-01", duration_length)

    iso8601 <- lubridate::interval(iso8601, tzone = tzone)

    date_time_start_not_na <- date_time_start[!date_time_start_is_na]

    date_time_end_not_na <- date_time_end[!date_time_end_is_na]

    iso8601_not_na <- lubridate::interval(
      date_time_start_not_na, date_time_end_not_na
    )

    iso8601_na <- lubridate::as.interval(NA_integer_)

    iso8601[!duration_is_na] <- iso8601_not_na

    iso8601[duration_is_na] <- iso8601_na

    iso8601 <- lubridate::format_ISO8601(iso8601, usetz = TRUE)

    hour_start <- df[[hour_start]]

    minute_start <- df[[minute_start]]

    hour_end <- df[[hour_end]]

    minute_end <- df[[minute_end]]

    hour_start_is_na <- is.na(hour_start)

    minute_start_is_na <- is.na(minute_start)

    no_start_time <- hour_start_is_na & minute_start_is_na

    hour_end_is_na <- is.na(hour_end)

    minute_end_is_na <- is.na(minute_end)

    no_end_time <-  hour_end_is_na & minute_end_is_na

    no_time <- no_start_time | no_end_time

    date_start <- df[[date_start]]

    date_end <- df[[date_end]]

    date_start_ymd <- lubridate::ymd(date_start)

    date_end_ymd <- lubridate::ymd(date_end)

    interval <- lubridate::interval(date_start_ymd, date_end_ymd)

    format_interval <- lubridate::format_ISO8601(interval, usetz = TRUE)

    iso8601 <- ifelse(no_time, format_interval, iso8601)

    no_duration <- date_time_start == date_time_end

    dates_equal <- date_start == date_end

    date_end_is_na <- is.na(date_end)

    no_end_date <- date_end_is_na | dates_equal

    no_end <- no_end_time & no_end_date

    use_start <- no_end | no_duration

    format_start <- lubridate::format_ISO8601(date_time_start, usetz = TRUE)

    iso8601 <- ifelse(use_start, format_start, iso8601)

    use_start_ymd <- no_start_time & no_end_date

    format_start_ymd <- lubridate::format_ISO8601(date_start_ymd, usetz = TRUE)

    iso8601 <- ifelse(use_start_ymd, format_start_ymd, iso8601)

    iso8601_is_na <- is.na(iso8601)

    date_interval <- paste(date_start, date_end, sep = "/")

    date_interval <- ifelse(dates_equal, date_start, date_interval)

    iso8601 <- ifelse(iso8601_is_na, date_interval, iso8601)

    fb_occurrence_df[[iso8601_var]] <- iso8601

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

  authors <- ifelse(
    nchar(authors) > 1L & !is.na(authors), paste0(" ", authors), ""
  )

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
