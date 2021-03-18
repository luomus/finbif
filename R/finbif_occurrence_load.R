#' Load FinBIF occurrence records from a file
#'
#' Load occurrence data from a file as a `data.frame`.
#'
#' @aliases fb_occurrence_load
#'
#' @param x Character or Integer. Either the path to a Zip archive or a TSV data
#'   file that has been downloaded from "laji.fi". Or a URI pointing to the data
#'   file (e.g., `http://tun.fi/HBF.49381`) or the integer representing the URI
#'   (i.e., `49381`).
#' @inheritParams finbif_records
#' @inheritParams finbif_occurrence
#' @return A `data.frame`. If `count_only =  TRUE` an integer.
#' @importFrom curl curl_download
#' @importFrom utils hasName read.delim
#' @importFrom methods as
#' @export

finbif_occurrence_load <- function(
  x, select, n, count_only = FALSE, quiet = FALSE, dwc = FALSE,
  date_time_method, tzone = getOption("finbif_tz"),
  locale = getOption("finbif_locale")
) {

  df <- read_finbif_tsv(x, n, count_only, quiet)

  if (count_only) return(df)

  n_recs <- attr(df, "nrow")

  df <- fix_issue_vars(df)

  var_type <- col_type_string(dwc)

  defer_errors({
    select <- infer_selection("none", select, var_type)
  })

  select <- select[["user"]]

  names(df) <- cite_file_vars[names(df), var_type]

  record_id <- df[["record_id"]]

  date_time_method <- det_datetime_method(date_time_method, n_recs)

  df <- compute_date_time(
    df, select, select, aggregate = "none", dwc, date_time_method, tzone
  )

  if (!utils::hasName(df, "any_issues")) {

    rec_iss <- !is.na(df[["record_issue"]])
    ev_iss  <- !is.na(df[["event_issue"]])
    tm_iss  <- !is.na(df[["time_issue"]])
    loc_iss <- !is.na(df[["location_issue"]])

    df[["any_issues"]] <- rec_iss | ev_iss | tm_iss | loc_iss

  }

  df <- compute_vars_from_id(df, select)

  for (extra_var in setdiff(select, names(df))) {

    ind <- var_names[["translated_var"]] == extra_var
    df[[extra_var]] <- methods::as(NA, var_names[ind, "type"])

  }

  structure(
    df[, select],
    class     = c("finbif_occ", "data.frame"),
    nrec_dnld = n_recs,
    nrec_avl  = n_recs,
    url       = "??",
    time      = "??",
    dwc       = dwc,
    record_id = record_id
  )

}

read_finbif_tsv <- function(x, n, count_only, quiet) {

  all <- missing(n)

  if (all) n <- -1L

  x <- as.character(x)

  tsv <- basename(x)
  tsv <- gsub("zip", "tsv", tsv)
  tsv <- paste0("rows_", tsv)

  if (grepl("^[0-9]*$", x)) {

    url <- sprintf("https://dw.laji.fi/download/HBF.%s", x)

    tsv <- sprintf("rows_HBF.%s.tsv", x)

    x <- tempfile()

    curl::curl_download(url, x, quiet)

    if (!quiet) message("")

  }

  con <- unz(x, tsv)

  warn <- getOption("warn")
  on.exit(options(warn = warn))
  options(warn = 2L)

  for (i in list(con, x)) {

    df <- try(
      if (count_only) {

        if (all) {

          nlines(i)

        } else {

          n

        }

      } else {

        if (!all) {

          n_rows <- nlines(i)

        }

        utils::read.delim(i, nrows = n, na.strings = "", quote = "")

      },
      silent = TRUE
    )

    try(close(i), silent = TRUE)

    success <- !inherits(df, "try-error")

    if (success) break

  }

  stopifnot("invalid file!" = success)

  if (count_only) return(df)

  if (all) {

    attr(df, "nrow") <- nrow(df)

  } else {

    attr(df, "nrow") <- n_rows

  }

  df

}

fix_issue_vars <- function(df) {

  type <- c("Time", "Location")

  for (i in c("Issue", "Source", "Message")) {
    for (j in 1:2) {
      names(df) <- gsub(
        sprintf("Issue.%s.%s", i, j),
        sprintf("%sIssue.%s", type[j], i),
        names(df)
      )
    }
  }

  df

}
