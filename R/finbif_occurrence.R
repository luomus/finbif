#' Download FinBIF occurrence records
#'
#' Download filtered occurrence data from FinBIF as a `data.frame`.
#'
#' @param ... Character vectors or list of character vectors. Taxa of records
#'   to download.
#' @inheritParams finbif_records
#' @param check_taxa Logical. Check first that taxa are in the FinBIF database.
#'   If true only records that match known taxa (have a valid taxon ID) are
#'   returned.
#' @param on_check_fail Character. What to do if a taxon is found not valid. One
#'   of `"warn"` (default), `"error"` or `"continue"`.
#' @param date_time Logical. Convert raw date and time variables into date-time
#'   and duration.
#' @param date_time_method Character. Passed to `lutz::tz_lookup_coords()` when
#'   `date_time = TRUE`. Default is `"fast"`. Use
#'   `date_time_method = "accurate"` (requires package `sf`) for greater
#'   accuracy.
#' @param tzone Character. If `date_time = TRUE` the timezone of outputted
#'   date-time. Defaults to system timezone.
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

finbif_occurrence <- function(..., filter, select, n = 10, page = 1,
  count_only = FALSE, quiet = FALSE, cache = TRUE, check_taxa = TRUE,
  on_check_fail = c("warn", "error", "quiet"), date_time = TRUE,
  date_time_method = "fast", tzone = Sys.timezone()
  ) {

  taxa <- list(...)
  ntaxa <- length(taxa)
  on_check_fail <- match.arg(on_check_fail)

  if (ntaxa)
    if (check_taxa) {

      taxa <- if (ntaxa > 1L || !utils::hasName(taxa, "taxa")) {
        unlist(finbif_check_taxa(taxa, cache = cache))
      } else {
        unlist(finbif_check_taxa(..., cache = cache))
      }

      if (anyNA(taxa)) {
        msg  <- paste(
          "Cannot find taxa:",
          paste(sub("\\.", " - ", names(taxa[is.na(taxa)])), collapse = ", ")
        )
        switch(
          on_check_fail, warn = warning(msg), error = stop(msg), quiet = NULL
        )
      }

      taxa <- list(taxon_id = paste(taxa[!is.na(taxa)], collapse = ","))

    } else {

      taxa <- list(taxon_name = paste(taxa, collapse = ","))

    }

  if (missing(filter)) filter <- NULL
  filter <- c(taxa, filter)

  records <- finbif_records(filter, select, n, page, count_only, quiet, cache)

  if (count_only) return(records[["content"]][["total"]])

  if (!quiet) pb_head("Processing data")

  df   <- as.data.frame(records, quiet = quiet)
  url  <- attr(df, "url", TRUE)
  time <- attr(df, "time", TRUE)

  # When any two datasets are requested, depending on the data they contain,
  # they may not have the same column classes. This will make them hard to
  # combine even if they apparently have the same columns.
  for (col in names(df))
    if (!is.list(df[[col]]))
      df[[col]] <- methods::as(df[[col]], var_names[col, "type"])

  names(df) <- var_names[names(df), "translated_var"]

  if (date_time) {
    df[["date_time"]] <- get_date_time(
      df, "date_start", "hour_start", "minute_start", "lat_wgs84", "lon_wgs84",
      date_time_method, tzone
    )
    df[["duration"]] <- get_duration(
      df, "date_time", "date_end", "hour_end", "minute_end", "lat_wgs84",
      "lat_wgs84", date_time_method, tzone
    )
  }

  structure(
    df,
    class     = c("finbif_occ", "data.frame"),
    nrec_dnld = attr(records, "nrec_dnld", TRUE),
    nrec_avl  = attr(records, "nrec_avl", TRUE),
    url       = url,
    time      = time
  )

}

get_date_time <- function(df, date, hour, minute, lat, lon, method, tzone) {

  if (is.null(df[[date]])) return(NULL)

  date_time <- lubridate::ymd(df[[date]])
  date_time <- lubridate::as_datetime(date_time)

  if (!is.null(df[[hour]])) {
    lubridate::hour(date_time) <-
      ifelse(is.na(df[[hour]]), lubridate::hour(date_time), df[[hour]])
  }

  if (!is.null(df[[minute]])) {
    lubridate::minute(date_time) <-
      ifelse(is.na(df[[minute]]), lubridate::minute(date_time), df[[minute]])
  }

  if (is.null(df[[lat]]) || is.null(df[[lon]])) return(NULL)
  tz <- lutz::tz_lookup_coords(df[[lat]], df[[lon]], method, FALSE)
  lubridate::force_tzs(
    date_time, tzones = ifelse(is.na(tz), "", tz), tzone_out = tzone
  )
}

get_duration <-
  function(df, date_time, date, hour, minute, lat, lon, method, tzone) {

    if (is.null(df[[date_time]])) return(NULL)
    if (is.null(df[["date_end"]]) || is.null(df[["hour_end"]])) return(NULL)

    date_time_end <- get_date_time(
      df, "date_end", "hour_end", "minute_end", "lat_wgs84", "lon_wgs84",
      method, tzone
    )

    ans <- lubridate::interval(df[[date_time]], date_time_end)
    lubridate::as.duration(ans)

  }
