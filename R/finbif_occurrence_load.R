#' Load FinBIF occurrence records from a file
#'
#' Load occurrence data from a file as a `data.frame`.
#'
#' @aliases fb_occurrence_load
#'
#' @param file Character or Integer. Either the path to a Zip archive or a TSV
#'   data file that has been downloaded from "laji.fi". Or a URI pointing to the
#'   data file (e.g., [http://tun.fi/HBF.49381](http://tun.fi/HBF.49381)) or the
#'   integer representing the URI (i.e., `49381`).
#' @param select Character vector. Variables to return. If not specified, a
#'   default set of commonly used variables will be used. Use `"default_vars"`
#'   as a shortcut for this set. Variables can be deselected by prepending a `-`
#'   to the variable name. If only deselects are specified the default set of
#'   variables without the deselection will be returned. Use `"all"` to select
#'   all available variables in the file.
#' @param write_file Character. Path to write downloaded zip file to if `file`
#'   refers to a URI. Will be ignored if `getOption("finbif_cache_path")` is not
#'   `NULL`.
#' @inheritParams finbif_records
#' @inheritParams finbif_occurrence
#' @return A `data.frame`, or if `count_only =  TRUE` an integer.
#' @examples \dontrun{
#'
#' # Get occurrence data
#' finbif_occurrence_load(49381)
#'
#' }
#' @importFrom digest digest
#' @importFrom httr progress RETRY status_code write_disk
#' @importFrom utils hasName read.delim
#' @importFrom methods as
#' @export

finbif_occurrence_load <- function(
  file, select, n, count_only = FALSE, quiet = FALSE,
  cache = getOption("finbif_use_cache"), dwc = FALSE, date_time_method,
  tzone = getOption("finbif_tz"), locale = getOption("finbif_locale"),
  write_file = tempfile()
) {

  var_type <- col_type_string(dwc)

  select_all <- FALSE
  short <- FALSE

  if (!missing(select) && select %in% c("all", "short")) {

    short <- identical(select, "short")
    select <- "default_vars"
    select_all <- TRUE

  }

  defer_errors(select <- infer_selection("none", select, var_type))

  select <- select[["user"]]

  if (missing(n)) n <- -1L

  df <- read_finbif_tsv(file, n, count_only, quiet, cache, write_file)

  if (count_only) return(df)

  n_recs <- attr(df, "nrow")

  url <- attr(df, "url")

  df <- fix_issue_vars(df)

  df <- new_vars(df)

  record_id <- df[["Unit.UnitID"]]

  names(df) <- cite_file_vars[names(df), var_type]

  for (i in names(df)) {

    df[[i]] <- add_nas(df, i)

  }

  if (select_all) {

    select <- TRUE

  } else {

    date_time_method <- det_datetime_method(date_time_method, n_recs)

    df <- compute_date_time(
      df, select, select, aggregate = "none", dwc, date_time_method, tzone
    )

    df <- any_issues(df, var_type)

    df <- compute_vars_from_id(df, select)

    for (extra_var in setdiff(select, names(df))) {

      ind <- var_names[["translated_var"]] == extra_var
      df[[extra_var]] <- methods::as(NA, var_names[ind, "type"])

    }

  }

  df <- structure(
    df[, select],
    class     = c("finbif_occ", "data.frame"),
    nrec_dnld = n_recs,
    nrec_avl  = n_recs,
    url       = url,
    time      = "??",
    short_nms = short,
    dwc       = dwc && !short,
    record_id = record_id
  )

  if (short) {
    short_nms <- cite_file_vars[["shrtnm"]]
    names(short_nms) <- cite_file_vars[[var_type]]
    names(df) <- short_nms[names(df)]
  }

  df

}

read_finbif_tsv <- function(file, n, count_only, quiet, cache, write_file) {

  file <- as.character(file)

  ptrn <- "http://tun.fi/HBF."

  is_url <- grepl(ptrn, file, fixed = TRUE)

  if (is_url) file <- gsub(ptrn, "", file)

  tsv <- basename(file)
  tsv <- gsub("zip", "tsv", tsv)
  tsv <- paste0("rows_", tsv)

  if (grepl("^[0-9]*$", file)) {

    url <- sprintf("https://dw.laji.fi/download/HBF.%s", file)

    tsv <- sprintf("rows_HBF.%s.tsv", file)

    file <- get_zip(url, quiet, cache, write_file)

  } else {

    url <- file

  }

  attr(df, "url") <- url

  con <- unz(file, tsv)

  warn <- getOption("warn")
  on.exit(options(warn = warn))
  options(warn = 2L)

  all <- identical(n, -1L)

  for (i in list(con, file)) {

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

new_vars <- function(df) {

  nms_df <- names(df)

  ind <- cite_file_vars[["superseeded"]] == "FALSE"

  ss <- rownames(cite_file_vars[!ind, ])

  ss <- intersect(ss, nms_df)

  ss <- cite_file_vars[ss, "superseeded"]

  nms <- row.names(cite_file_vars[ind, ])

  new_vars <- setdiff(nms, c(nms_df, ss))

  for (i in new_vars) {

    df[[i]] <- NA

  }

  df

}

get_zip <- function(url, quiet, cache, write_file) {

  if (cache) {

    hash <- digest::digest(url)

    fcp <- getOption("finbif_cache_path")

    if (is.null(fcp)) {

      zip <- get_cache(hash)

      if (!is.null(zip)) {

        return(zip)

      }

      zip <- write_file

      on.exit(if (!is.null(zip)) set_cache(zip, hash))

    } else {

      zip <- file.path(fcp, paste0("finbif_cache_file_", hash))

      if (file.exists(zip)) {

        return(zip)

      }

    }

  }

  progress <- NULL

  if (!quiet) progress <- httr::progress()

  resp <- httr::RETRY(
    "GET", url, httr::write_disk(zip, overwrite = TRUE), progress
  )

  if (!quiet) message("")

  code <- httr::status_code(resp)

  if (!identical(code, 200L)) {

    stop(sprintf("File request failed [%s]", code), call. = FALSE)

  }

  zip

}

add_nas <- function(df, nm) {

  ans <- df[[nm]]

  if (all(is.na(ans))) {

    ind <- cite_file_vars[["translated_var"]] == nm

    if (length(which(ind)) > 1L) {

      ind <- ind & cite_file_vars[["superseeded"]] == "FALSE"

    }

    ans <- methods::as(ans, cite_file_vars[ind, "type"])

  }

  ans

}

any_issues <- function(df, var_type) {

  vnms <- var_names[var_type]
  any_issue <- vnms["unit.quality.documentGatheringUnitQualityIssues", ]

  if (!utils::hasName(df, any_issue)) {

    rec_iss <- !is.na(df[[vnms["unit.quality.issue.issue", ]]])
    ev_iss  <- !is.na(df[[vnms["gathering.quality.issue.issue", ]]])
    tm_iss  <- !is.na(df[[vnms["gathering.quality.timeIssue.issue", ]]])
    loc_iss <- !is.na(df[[vnms["gathering.quality.locationIssue.issue", ]]])

    df[[any_issue]] <- rec_iss | ev_iss | tm_iss | loc_iss

  }

  df

}
