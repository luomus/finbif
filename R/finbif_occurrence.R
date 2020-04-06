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
#'   `"fast"`. Use `date_time_method = "accurate"` (requires package `sf`) for
#'   greater accuracy.
#' @param check_taxa Logical. Check first that taxa are in the FinBIF database.
#'   If true only records that match known taxa (have a valid taxon ID) are
#'   returned.
#' @param on_check_fail Character. What to do if a taxon is found not valid. One
#'   of `"warn"` (default) or `"error"`.
#' @param tzone Character. If `date_time` has been selected the timezone of the
#'   outputted date-time. Defaults to system timezone.
#' @param dwc Logical. Return Darwin Core (or Darwin Core style) variable names.
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
#' @importFrom methods as
#' @importFrom utils hasName
#' @importFrom lubridate as_datetime as.duration force_tzs hour interval minute
#' @importFrom lubridate ymd
#' @importFrom lutz tz_lookup_coords
#' @export

finbif_occurrence <- function(
  ..., filter, select, order_by, sample = FALSE, n = 10, page = 1,
  count_only = FALSE, quiet = FALSE, cache = getOption("finbif_use_cache"),
  date_time_method = "fast", check_taxa = TRUE,
  on_check_fail = c("warn", "error"), tzone = getOption("finbif_tz"),
  dwc = FALSE
) {

  taxa <- select_taxa(..., cache = cache, check_taxa = check_taxa,
    on_check_fail = match.arg(on_check_fail)
  )

  if (missing(filter)) filter <- NULL
  filter <- c(taxa, filter)

  records <- finbif_records(
    filter, select, order_by, sample, n, page, count_only, quiet, cache
  )

  if (count_only) return(records[["content"]][["total"]])

  # Don't need a processing progress bar if only one page of records
  if (length(records) < 2L) quiet <- TRUE

  if (!quiet) pb_head("Processing data")

  df   <- as.data.frame(records, quiet = quiet)
  url  <- attr(df, "url", TRUE)
  time <- attr(df, "time", TRUE)

  names(df) <- var_names[names(df), if (dwc) "dwc" else "translated_var"]

  select_ <- attr(records, "select_user")
  if (dwc)
    select_ <- var_names[match(select_, var_names[["translated_var"]]), "dwc"]

  date_time <-
    missing(select) ||
    any(c("default_vars", "date_time", "duration") %in% select)

  if (date_time)
    if (dwc) {
      df[["eventDateTime"]] <- get_date_time(
        df, "eventDateStart", "hourStart", "minuteStart",
        "decimalLatitude", "decimalLongitude", date_time_method, tzone
      )
      if ("samplingEffort" %in% select_)
        df[["samplingEffort"]] <- get_duration(
          df, "eventDateTime", "eventDateStart", "hourStart",
          "minuteStart", "decimalLatitude", "decimalLongitude",
          date_time_method, tzone
        )
    } else {
      df[["date_time"]] <- get_date_time(
        df, "date_start", "hour_start", "minute_start", "lat_wgs84",
        "lon_wgs84", date_time_method, tzone
      )
      if ("duration" %in% select_)
        df[["duration"]] <- get_duration(
          df, "date_time", "date_end", "hour_end", "minute_end", "lat_wgs84",
          "lon_wgs84", date_time_method, tzone
        )
    }

  structure(
    df[select_],
    class     = c("finbif_occ", "data.frame"),
    nrec_dnld = attr(records, "nrec_dnld", TRUE),
    nrec_avl  = attr(records, "nrec_avl", TRUE),
    url       = url,
    time      = time,
    dwc       = dwc
  )

}

select_taxa <- function(..., cache, check_taxa, on_check_fail) {

  taxa <- list(...)
  ntaxa <- length(taxa)
  ans <- list(taxon_name = paste(taxa, collapse = ","))

  if (ntaxa && check_taxa) {

    taxa <-
      if (ntaxa > 1L || !utils::hasName(taxa, "taxa")) {
        unlist(finbif_check_taxa(taxa, cache = cache))
      } else {
        unlist(finbif_check_taxa(..., cache = cache))
      }

    taxa_invalid <- is.na(taxa)

    if (any(taxa_invalid)) {
      msg  <- paste(
        "Cannot find taxa:",
        paste(sub("\\.", " - ", names(taxa[taxa_invalid])), collapse = ", ")
      )
      switch(
        on_check_fail,
        warn  = warning(msg, call. = FALSE),
        error = stop(msg, call. = FALSE)
      )
    }

    if (!all(taxa_invalid))
      ans <- list(taxon_id = paste(taxa[!taxa_invalid], collapse = ","))

  }

  ans

}

get_date_time <- function(df, date, hour, minute, lat, lon, method, tzone) {

  date_time <- lubridate::ymd(df[[date]])
  date_time <- lubridate::as_datetime(date_time)

  # When there is no hour assume the hour is midday (i.e., don't assume
  # midnight)
  lubridate::hour(date_time) <- 12L

  if (!is.null(df[[hour]]))
    lubridate::hour(date_time) <-
      ifelse(is.na(df[[hour]]), lubridate::hour(date_time), df[[hour]])

  if (!is.null(df[[minute]]))
    lubridate::minute(date_time) <-
      ifelse(is.na(df[[minute]]), lubridate::minute(date_time), df[[minute]])

  tz <- lutz::tz_lookup_coords(df[[lat]], df[[lon]], method, FALSE)
  lubridate::force_tzs(
    date_time, tzones = ifelse(is.na(tz), "", tz), tzone_out = tzone
  )
}

get_duration <-
  function(df, date_time, date, hour, minute, lat, lon, method, tzone) {

    date_time_end <-
      get_date_time(df, date, hour, minute, lat, lon, method, tzone)

    ans <- lubridate::interval(df[[date_time]], date_time_end)
    ans <- ifelse(is.na(df[[minute]]) | is.na(df[[hour]]), NA, ans)
    lubridate::as.duration(ans)

  }
