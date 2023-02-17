#' Load FinBIF occurrence records from a file
#'
#' Load occurrence data from a file as a `data.frame`.
#'
#' @aliases fb_occurrence_load
#'
#' @param file Character or Integer. Either the path to a Zip archive or
#'   tabular data file that has been downloaded from "laji.fi", a URI
#'   linking to such a data file (e.g.,
#'   [https://tun.fi/HBF.49381](https://tun.fi/HBF.49381)) or an integer
#'   representing the URI (i.e., `49381`).
#' @param select Character vector. Variables to return. If not specified, a
#'   default set of commonly used variables will be used. Use `"default_vars"`
#'   as a shortcut for this set. Variables can be deselected by prepending a `-`
#'   to the variable name. If only deselects are specified the default set of
#'   variables without the deselection will be returned. Use `"all"` to select
#'   all available variables in the file.
#' @param n Integer. How many records to import. Negative and other invalid
#'   values are ignored causing all records to be imported.
#' @param write_file Character. Path to write downloaded zip file to if `file`
#'   refers to a URI. Will be ignored if `getOption("finbif_cache_path")` is not
#'   `NULL` and will use the cache path instead.
#' @param dt Logical. If package, `data.table`, is available return a
#'   `data.table` object rather than a `data.frame`.
#' @param keep_tsv Logical. Whether to keep the TSV file if `file` is a ZIP
#'   archive or represents a URI. Is ignored if `file` is already a TSV. If
#'   `TRUE` the tsv file will be kept in the same directory as the ZIP archive.
#' @param facts List. A named list of "facts" to extract from supplementary
#'   "fact" files in a local or online FinBIF data archive. Names can include
#'   one or more of `"record"`, `"event"` or `"document"`. Elements of the list
#'   are character vectors of the "facts" to be extracted and then joined to the
#'   return value.
#' @param type_convert_facts Logical. Should facts be converted from character
#'   to numeric or integer data where applicable?
#' @param drop_facts_na Logical. Should missing or "all `NA`" facts be dropped?
#'   Any value other than a length one logical vector with the value of TRUE
#'   will be interpreted as FALSE. Argument is ignored if `drop_na` is TRUE for
#'   all variables explicitly or via recycling. To only drop some
#'   missing/`NA`-data facts use `drop_na` argument.
#' @param locale Character. One of the supported two-letter ISO 639-1 language
#'   codes. Current supported languages are English, Finnish, Swedish, Russian,
#'   and Sámi (Northern). For data where more than one language is available
#'   the language denoted by `locale` will be preferred while falling back to
#'   the other languages in the order indicated above.
#' @param skip Integer. The number of lines of the data file to skip before
#'   beginning to read data (not including the header).
#' @inheritParams finbif_records
#' @inheritParams finbif_occurrence
#' @return A `data.frame`, or if `count_only =  TRUE` an integer.
#' @examples \dontrun{
#'
#' # Get occurrence data
#' finbif_occurrence_load(49381)
#'
#' }
#' @export

finbif_occurrence_load <- function(
  file,
  select = NULL,
  n = -1,
  count_only = FALSE,
  quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"),
  dwc = FALSE,
  date_time_method = NULL,
  tzone = getOption("finbif_tz"),
  write_file = tempfile(),
  dt = NA,
  keep_tsv = FALSE,
  facts = list(),
  type_convert_facts = TRUE,
  drop_na = FALSE,
  drop_facts_na = drop_na,
  locale = getOption("finbif_locale"),
  skip = 0
) {

  file <- preprocess_data_file(file)

  n <- as.integer(n)

  fb_records_obj <- list(
    file = file,
    n = n,
    count_only = count_only,
    quiet = quiet,
    cache = cache,
    write_file = write_file,
    dt = dt,
    keep_tsv = keep_tsv,
    skip = skip,
    facts = "none"
  )

  var_type <- col_type_string(dwc)

  short <- FALSE

  deselect <- character()

  all_cols <- c("all", "short")

  all_cols <- all_cols %in% select

  all_cols <- any(all_cols)

  var_names <- var_names()

  col_names <- var_names[[var_type]]

  if (all_cols) {

    select_one <- select[[1L]]

    short <- identical(select_one, "short")

    deselect <- grep("^-", select, value = TRUE)

    deselect <- gsub("-", "", deselect)

    deselect <- match(deselect, col_names)

    deselect <- var_names[deselect, ]

    deselect <- row.names(deselect)

    select <- "default_vars"

  }

  fb_records_obj[["aggregate"]] <- "none"

  fb_records_obj[["select"]] <- select

  fb_records_obj[["include_facts"]] <- FALSE

  fb_records_obj[["var_type"]] <- var_type

  defer_errors({

    select <- infer_selection(fb_records_obj)

  })

  fact_types <- vapply(facts, length, 0L)

  fact_types <- fact_types > 0L

  fact_types <- which(fact_types)

  fact_types <- names(fact_types)

  select[["deselect"]] <- deselect

  select[["all"]] <- all_cols

  select[["type"]] <- var_type

  select[["facts"]] <- fact_types

  fb_records_obj[["select"]] <- select

  fb_occurrence_df <- read_finbif_tsv(fb_records_obj)

  if (count_only) {

    return(fb_occurrence_df)

  }

  file_vars <- attr(fb_occurrence_df, "file_vars", TRUE)

  attr(file_vars, "var_type") <- var_type

  select[["lite"]] <- attr(file_vars, "lite", TRUE)

  attr(fb_occurrence_df, "locale") <- locale

  fb_occurrence_df <- localise_enums(fb_occurrence_df)

  n_recs <- attr(fb_occurrence_df, "nrow", TRUE)

  url <- attr(fb_occurrence_df, "url", TRUE)

  df_names <- names(fb_occurrence_df)

  df_names <- fix_issue_vars(df_names)

  names(fb_occurrence_df) <- df_names

  select <- select_facts(select)

  fact_types <- select[["facts"]]

  attr(fb_occurrence_df, "select") <- select

  fb_occurrence_df <- new_vars(fb_occurrence_df)

  translated_vars <- file_vars[["translated_var"]]

  record_id <- translated_vars == "record_id"

  record_id <- file_vars[record_id, ]

  record_id <- rownames(record_id)

  record_id <- fb_occurrence_df[[record_id]]

  fb_occurrence_df <- expand_lite_cols(fb_occurrence_df)

  df_names <- names(fb_occurrence_df)

  nms <- file_vars[df_names, var_type]

  na_nms <- is.na(nms)

  df_names <- ifelse(na_nms, df_names, nms)

  names(fb_occurrence_df) <- df_names

  select_user <- select[["user"]]

  fb_occurrence_df <- structure(
    fb_occurrence_df,
    select_user = select_user,
    column_names = select_user,
    aggregate = "none",
    dwc = dwc,
    date_time_method = date_time_method,
    tzone = tzone,
    locale = locale,
    include_new_cols = !all_cols,
    record_id = record_id
  )

  fb_occurrence_df <- compute_vars_from_id(fb_occurrence_df)

  fb_occurrence_df <- compute_abundance(fb_occurrence_df)

  fb_occurrence_df <- compute_citation(fb_occurrence_df)

  fb_occurrence_df <- compute_coordinate_uncertainty(fb_occurrence_df)

  fb_occurrence_df <- compute_scientific_name(fb_occurrence_df)

  fb_occurrence_df <- add_nas(fb_occurrence_df)

  df_names <- names(fb_occurrence_df)

  select[["user"]] <- df_names

  if (!all_cols) {

    select[["user"]] <- select_user

    datetime_obj <- list(date_time_method = date_time_method, n = n_recs)

    datetime_obj <- det_datetime_method(datetime_obj)

    date_time <- select[["date_time_selected"]]

    date_time_method <- datetime_obj[["date_time_method"]]

    fb_occurrence_df <- structure(
      fb_occurrence_df,
      select_user = select_user,
      column_names = select_user,
      aggregate = "none",
      dwc = dwc,
      date_time = date_time,
      date_time_method = date_time_method,
      tzone = tzone
    )

    fb_occurrence_df <- date_times(fb_occurrence_df)

    fb_occurrence_df <- compute_date_time(fb_occurrence_df)

    fb_occurrence_df <- compute_duration(fb_occurrence_df)

    fb_occurrence_df <- compute_iso8601(fb_occurrence_df)

    fb_occurrence_df <- any_issues(fb_occurrence_df)

    df_names <- names(fb_occurrence_df)

    extra_vars <- setdiff(select_user, df_names)

    for (extra_var in extra_vars) {

      ind <- col_names == extra_var

      type <- var_names[ind, "type"]

      nrows <- nrow(fb_occurrence_df)

      na <- rep_len(NA, nrows)

      na <- cast_to_type(na, type)

      fb_occurrence_df[[extra_var]] <- na

    }

  }

  attr(fb_occurrence_df, "file_cols") <- NULL

  attr(fb_occurrence_df, "file_vars") <- NULL

  class <- class(fb_occurrence_df)

  class <- c("finbif_occ", class)

  dwc <- dwc && !short

  fb_occurrence_df <- structure(
    fb_occurrence_df,
    class = class,
    nrec_dnld = n_recs,
    nrec_avl = n_recs,
    url = url,
    time = "??",
    short_nms = short,
    dwc = dwc,
    record_id = record_id
  )

  for (fact_type in fact_types) {

    levels <- c("record", "event", "document")

    level_ok <- fact_type %in% levels

    stopifnot("Invalid fact type" = level_ok)

    deselect <- character()

    select_obj <- list(all = TRUE, deselect, type = "translated_var")

    fb_records_obj[["select"]] <- select_obj

    fb_records_obj[["n"]] <- -1L

    fb_records_obj[["facts"]] <- fact_type

    facts_df <- try({

        read_finbif_tsv(fb_records_obj)

      },
      silent = TRUE
    )

    file_vars_type <- file_vars[, var_type, drop = FALSE]

    record_type <- file_vars_type["Unit.UnitID", ]

    event_type <- file_vars_type["Gathering.GatheringID", ]

    document_type <- file_vars_type["Document.DocumentID", ]

    id <- switch(
      fact_type,
      record = record_type,
      event = event_type,
      document = document_type
    )

    fact_lvl <- facts[[fact_type]]

    facts_df <- structure(
      facts_df,
      facts = fact_lvl,
      fact_type = fact_type,
      id = id,
      type_convert_facts = type_convert_facts,
      drop_facts_na = drop_facts_na
    )

    facts_df <- spread_facts(facts_df)

    select_user <- select[["user"]]

    df_user <- fb_occurrence_df[select_user]

    df_names_user <- names(df_user)

    df_names_facts <- names(facts_df)

    df_names_facts <- setdiff(df_names_facts, id)

    select_user <- c(df_names_user, df_names_facts)

    select[["user"]] <- select_user

    attr(fb_occurrence_df, "facts_df") <- facts_df

    fb_occurrence_df <- bind_facts(fb_occurrence_df)

  }

  if (short) {

    df_names <- names(fb_occurrence_df)

    short_nms <- short_nms(file_vars)

    short_nms <- short_nms[df_names]

    short_fcts <- grep("_fact__", df_names, value = TRUE)

    short_fcts <- sub("^.*_fact__", "", short_fcts)

    short_fcts <- sub("http://tun.fi/", "", short_fcts)

    n <- length(short_fcts)

    n <- n + .1

    n <- log10(n)

    n <- ceiling(n)

    n <- 9 - n

    short_fcts <- abbreviate(
      short_fcts, n, FALSE, strict = TRUE, method = "both.sides"
    )

    short_fact_sq <- seq_along(short_fcts)

    short_fcts <- paste0("f", short_fact_sq, short_fcts)

    short_nms_na <- is.na(short_nms)

    short_nms[short_nms_na] <- short_fcts

    missing <- which(short_nms == "f")

    nms_missing <- df_names[missing]

    nms_missing <- gsub("[^A-Za-z]", "", nms_missing)

    nms_missing <- abbreviate(nms_missing, 10L)

    short_nms[missing] <- nms_missing

    df_names <- short_nms

    select[["user"]] <- df_names

    names(fb_occurrence_df) <- df_names

  }

  select_user <- select[["user"]]

  select_user <- name_chr_vec(select_user)

  fb_occurrence_df <- fb_occurrence_df[, select_user, drop = FALSE]

  attr(fb_occurrence_df, "column_names") <- select_user

  attr(fb_occurrence_df, "drop_na") <- drop_na

  drop_na_col(fb_occurrence_df)

}

#' @noRd

read_finbif_tsv <- function(fb_occurrenc_obj) {

  n <- fb_occurrenc_obj[["n"]]

  count_only <- fb_occurrenc_obj[["count_only"]]

  facts <- fb_occurrenc_obj[["facts"]]

  ptrn <- "^https?://.+?/HBF\\."

  file <- fb_occurrenc_obj[["file"]]

  file <- as.character(file)

  is_url <- grepl(ptrn, file)

  if (is_url) {

    file <- sub(ptrn, "", file)

  }

  tsv <- basename(file)

  tsv <- gsub("zip", "tsv", tsv)

  tsv_prefix <- switch(
    facts,
    none = "rows_",
    record = "unit_facts_",
    event = "gathering_facts_",
    document = "document_facts_",
  )

  fact_types <- c("none", "record", "event", "document")

  valid_facts <- facts %in% fact_types

  stopifnot(
    "Facts can only be of types: record, event and/or document" = valid_facts
  )

  file <- gsub("rows_", tsv_prefix, file)

  fb_occurrenc_obj[["file"]] <- file

  tsv <- paste0(tsv_prefix, tsv)

  fb_occurrenc_obj[["tsv"]] <- tsv

  is_number <- grepl("^[0-9]*$", file)

  if (is_number) {

    finbif_dl_url <- getOption("finbif_dl_url")

    url <- sprintf("%s/HBF.%s", finbif_dl_url, file)

    fb_occurrenc_obj[["url"]] <- url

    tsv <- sprintf("%sHBF.%s.tsv", tsv_prefix, file)

    fb_occurrenc_obj[["tsv"]] <- tsv

    fb_occurrenc_obj <- get_zip(fb_occurrenc_obj)

    file <- fb_occurrenc_obj[["file"]]

  } else {

    url <- file

  }

  df <- attempt_read(fb_occurrenc_obj)

  if (count_only) {

    return(df)

  }

  attr(df, "url") <- url

  n_all <- identical(n, -1L)

  if (n_all) {

    attr(df, "nrow") <- nrow(df)

  }

  df

}

#' @noRd

attempt_read <- function(fb_occurrence_obj) {

  file <- fb_occurrence_obj[["file"]]

  tsv <- fb_occurrence_obj[["tsv"]]

  count_only <- fb_occurrence_obj[["count_only"]]

  n <- fb_occurrence_obj[["n"]]

  dt <- fb_occurrence_obj[["dt"]]

  use_dt <- dt

  dt_na <- is.na(dt)

  if (dt_na) {

    use_dt <- TRUE

    fb_occurrence_obj[["dt"]] <- FALSE

  }

  if (count_only) {

    nlines(fb_occurrence_obj)

  } else {

    n_rows <- NULL

    has_n <- !identical(n, -1L)

    if (has_n) {

      n_rows <- nlines(fb_occurrence_obj)

    }

    use_dt <- use_dt && has_pkgs("data.table")

    if (use_dt) {

      input <- as.character(file)

      is_tsv <- grepl("\\.tsv$", input)

      input_list <- list(input = input, tsv = tsv)

      input_list <- list(zip = input_list)

      if (is_tsv) {

        fb_occurrence_obj[["keep_tsv"]] <- FALSE

        input_list <- list(input = input)

      }

      fb_occurrence_obj[["dt_args"]] <- input_list

      df <- dt_read(fb_occurrence_obj)

    } else {

      df <- rd_read(fb_occurrence_obj)

    }

    attr(df, "nrow") <- n_rows

    df

  }

}

#' @noRd

localise_enums <- function(df) {

  file_vars <- attr(df, "file_vars", TRUE)

  filed_var_names <- row.names(file_vars)

  locale <- attr(df, "locale", TRUE)

  df_names <- names(df)

  for (nm in df_names) {

    localised <- file_vars[[nm, "localised"]]

    localised <- isTRUE(localised)

    localised <- localised && nm %in% filed_var_names

    if (localised) {

      dfnm <- df[[nm]]

      labels_obj <- list(
        labels = dfnm, col = nm, var_names = file_vars, locale = locale
      )

      dfnm <- localise_labels(labels_obj)

      df[[nm]] <- dfnm

    }

  }

  df

}

#' @noRd

fix_issue_vars <- function(x) {

  type <- c("Time", "Location")

  cols <- c("Issue", "Source", "Message")

  sq <- seq_len(2L)

  for (i in cols) {

    for (j in sq) {

      type_j <- type[[j]]

      issue <- sprintf("Issue.%s.%s", i, j)

      issue_type <- sprintf("%sIssue.%s", type_j, i)

      x <- sub(issue, issue_type, x)

    }

  }

  x

}

#' @noRd

new_vars <- function(df) {

  file_vars <- attr(df, "file_vars", TRUE)

  select <- attr(df, "select", TRUE)

  deselect <- select[["deselect"]]

  add <- !select[["all"]]

  file_cols <- attr(df, "file_cols", TRUE)

  no_file_cols <- is.null(file_cols)

  if (no_file_cols) {

    file_cols <- names(df)

    attr(df, "file_cols") <- file_cols

  }

  superseeded <- file_vars[["superseeded"]]

  ind <- superseeded == "FALSE"

  ss <- file_vars[!ind, ]

  ss <- rownames(ss)

  ss <- intersect(ss, file_cols)

  ss <- file_vars[ss, "superseeded"]

  var_names <- var_names()

  deselect <- var_names[deselect, "translated_var"]

  translated_vars <- file_vars[["translated_var"]]

  deselect <- translated_vars %in% deselect

  ind <- ind & !deselect

  nms <- file_vars[ind, ]

  nms <- row.names(nms)

  file_cols <- c(file_cols, ss)

  new_vars <- setdiff(nms, file_cols)

  if (add) {

    nrows <- nrow(df)

    dfi <- rep_len(NA, nrows)

    for (i in new_vars) {

      df[[i]] <- dfi

    }

  }

  df

}

#' @noRd
#' @importFrom digest digest
#' @importFrom httr RETRY progress write_disk

get_zip <- function(fb_occurrenc_obj) {

  url <- fb_occurrenc_obj[["url"]]

  quiet <- fb_occurrenc_obj[["quiet"]]

  cache <- fb_occurrenc_obj[["cache"]]

  write_file <- fb_occurrenc_obj[["write_file"]]

  if (cache) {

    hash <- sub(":\\d+", "", url)

    hash <- digest::digest(hash)

    fcp <- getOption("finbif_cache_path")

    fcp_is_null <- is.null(fcp)

    if (fcp_is_null) {

      cache_file <- get_cache(hash)

      has_cache_file <- !is.null(cache_file)

      if (has_cache_file) {

        fb_occurrenc_obj[["file"]] <- cache_file

        return(fb_occurrenc_obj)

      }

      on.exit({

        has_write_file <- !is.null(write_file)

        if (has_write_file) {

          cache_obj <- list(data = write_file, hash = hash)

          set_cache(cache_obj)

        }

      })

    } else {

      file_name <- paste0("finbif_cache_file_", hash)

      write_file <- file.path(fcp, file_name)

      write_file_exists <- file.exists(write_file)

      if (write_file_exists) {

        fb_occurrenc_obj[["file"]] <- write_file

        return(fb_occurrenc_obj)

      }

    }

  }

  progress <- NULL

  if (!quiet) {

    progress <- httr::progress()

  }

  allow <- getOption("finbif_allow_query")

  stopifnot("Request not cached and option:finbif_allow_query = FALSE" = allow)

  rate_limit <- getOption("finbif_rate_limit")

  sleep <- 1 / rate_limit

  Sys.sleep(sleep)

  query <- list()

  auth <- Sys.getenv("FINBIF_RESTRICTED_FILE_ACCESS_TOKEN")

  has_auth <- !identical(auth, "")

  if (has_auth) {

    query <- list(personToken = auth)

  }

  write_disk <- httr::write_disk(write_file, overwrite = TRUE)

  times <- getOption("finbif_retry_times")

  pause_base <- getOption("finbif_retry_pause_base")

  pause_cap <- getOption("finbif_retry_pause_cap")

  pause_min <- getOption("finbif_retry_pause_min")

  resp <- httr::RETRY(
    "GET",
    url,
    write_disk,
    progress,
    query = query,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap,
    pause_min = pause_min,
    quiet = quiet,
    terminate_on = 404L
  )

  fs <- file.size(write_file)

  fl <- Sys.getenv("FINBIF_FILE_SIZE_LIMIT")

  fl <- as.integer(fl)

  too_large <- isTRUE(fs > fl)

  if (too_large) {

    stop("File download too large; err_name: too_large", call. = FALSE)

  }

  if (!quiet) {

    message("")

  }

  code <- resp[["status_code"]]

  has_error <- !identical(code, 200L)

  if (has_error) {

    msg <- sprintf("File request failed [%s]; err_name: request_failed", code)

    stop(msg, call. = FALSE)

  }

  fb_occurrenc_obj[["file"]] <- write_file

  fb_occurrenc_obj

}

#' @noRd

add_nas <- function(df) {

  dwc <- attr(df, "dwc", TRUE)

  var_type <- col_type_string(dwc)

  nms <- names(df)

  for (nm in nms) {

    ans <- df[[nm]]

    is_na <- is.na(ans)

    all_na <- all(is_na)

    if (all_na) {

      file_vars <- attr(df, "file_vars", TRUE)

      file_var_type <- file_vars[[var_type]]

      ind <- file_var_type == nm

      ind_int <- which(ind)

      superseeded <- file_vars[["superseeded"]]

      not_superseeded <- superseeded == "FALSE"

      ind <- ind & not_superseeded

      l <- length(ind_int)

      no_nm <- identical(l, 0L)

      if (no_nm) {

        file_vars <- var_names()

        file_var_type <- file_vars[[var_type]]

        ind <- file_var_type == nm

      }

      type <- file_vars[ind, "type"]

      df_nm <- cast_to_type(ans, type)

      df[[nm]] <- df_nm

    }

  }

  df

}

#' @noRd

any_issues <- function(fb_occurrence_df) {

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  var_type <- col_type_string(dwc)

  any_issue <- "unit.quality.documentGatheringUnitQualityIssues"

  var_names <- var_names()

  any_issue <- var_names[[any_issue, var_type]]

  select_user <- attr(fb_occurrence_df, "select_user", TRUE)

  df_nms <- names(fb_occurrence_df)

  needs_any_issue <- any_issue %in% select_user

  needs_any_issue <- needs_any_issue && !any_issue %in% df_nms

  if (needs_any_issue) {

    issue <- logical()

    nrows <- nrow(fb_occurrence_df)

    has_rows <- nrows > 0L

    if (has_rows) {

      issue <- FALSE

      issue_cols <- c(
        "unit.quality.issue.issue",
        "gathering.quality.issue.issue",
        "gathering.quality.timeIssue.issue",
        "gathering.quality.locationIssue.issue"
      )

      has_an_issue_col <- FALSE

      for (i in issue_cols) {

        issue_col <- var_names[[i, var_type]]

        issue_col <- fb_occurrence_df[[issue_col]]

        issue_col_len <- length(issue_col)

        has_issue_col <- issue_col_len > 0L

        if (has_issue_col) {

          issue_col <- !is.na(issue_col)

          issue <- issue | issue_col

        }

        has_an_issue_col <- has_an_issue_col | has_issue_col

      }

      if (!has_an_issue_col) {

        issue <- NA

      }

    }

    fb_occurrence_df[[any_issue]] <- issue

  }

  fb_occurrence_df

}

#' @noRd
#' @importFrom utils unzip

dt_read <- function(fb_occurrence_obj) {

  select <- fb_occurrence_obj[["select"]]

  n <- fb_occurrence_obj[["n"]]

  quiet <- fb_occurrence_obj[["quiet"]]

  dt <- fb_occurrence_obj[["dt"]]

  keep_tsv <- fb_occurrence_obj[["keep_tsv"]]

  skip <- fb_occurrence_obj[["skip"]]

  dt_args <- fb_occurrence_obj[["dt_args"]]

  args <- list(
    nrows = 0,
    showProgress = !quiet,
    data.table = dt,
    na.strings = "",
    quote = "",
    sep = "\t",
    fill = TRUE,
    check.names = FALSE,
    header = TRUE,
    skip = 0L
  )

  args <- c(dt_args, args)

  arg_names <- names(args)

  use_zip <- "zip" %in% arg_names

  if (use_zip) {

    unzip <- op_unzip()

    args_zip <- args[["zip"]]

    zip_input <- args_zip[["input"]]

    zip_tsv <- args_zip[["tsv"]]

    dir <- dirname(zip_input)

    args_input <- sprintf("%s/%s", dir, zip_tsv)

    args[["input"]] <- args_input

    input_exists <- file.exists(args_input)

    if (!input_exists) {

      utils::unzip(zip_input, files = zip_tsv, exdir = dir, unzip = unzip)

      if (!keep_tsv) {

        on.exit({

          unlink(args_input)

        })

      }

    }

    args[["zip"]] <- NULL

  }

  cols <- do.call(data.table::fread, args)

  cols <- names(cols)

  cols <- make.names(cols)

  cols <- make.unique(cols)

  cols <- fix_issue_vars(cols)

  file_vars <- infer_file_vars(cols)

  select[["file_vars"]] <- file_vars

  lite <- attr(file_vars, "lite", TRUE)

  if (lite) {

    args[["quote"]] <- "\""

  }

  select_all <- select[["all"]]

  if (select_all) {

    deselect <- deselect(select)

    args_select <- !cols %in% deselect

    args_select <- which(args_select)

    args[["select"]] <- args_select

  } else {

    select_query <- select[["query"]]

    iss <- "unit.quality.documentGatheringUnitQualityIssues"

    iss <- iss %in% select_query

    if (iss) {

      select_query <- c(
        select_query,
        "unit.quality.issue.issue",
        "gathering.quality.issue.issue",
        "gathering.quality.timeIssue.issue",
        "gathering.quality.locationIssue.issue"
      )

      select[["query"]] <- select_query

    }

    select_type <- select[["type"]]

    var_names <- var_names()

    select_vars <-  var_names[select_query, select_type]

    file_vars_type <- file_vars[[select_type]]

    select_vars <- file_vars_type %in% select_vars

    expand_vars <- c(
      "formatted_taxon_name",
      "formatted_date_time",
      "coordinates_euref",
      "coordinates_1_ykj",
      "coordinates_10_ykj",
      "coordinates_1_center_ykj",
      "coordinates_10_center_ykj"
    )

    translated_vars <- file_vars[["translated_var"]]

    expand_vars <- translated_vars %in% expand_vars

    expand_vars <- expand_vars | select_vars

    locale <- attr(file_vars, "locale", TRUE)

    select_vars <- switch(locale, none = select_vars, expand_vars)

    file_vars_select <- file_vars[select_vars, ]

    select_vars <- row.names(file_vars_select)

    args_select <- cols %in% select_vars

    args_select <- which(args_select)

    select_facts <- select[["facts"]]

    for (ftype in select_facts) {

      id_col <- switch(
        ftype,
        record = "Unit.UnitID",
        event = "Gathering.GatheringID",
        document = "Document.DocumentID"
      )

      id_col <- cols %in% id_col

      id_col <- which(id_col)

      args_select  <- c(args_select, id_col)

    }

    args_select <- unique(args_select)

    args_select <- sort(args_select)

    args[["select"]] <- args_select

  }

  args[["nrows"]] <- as.double(n)

  args[["check.names"]] <- TRUE

  skip <- skip + 1L

  args[["skip"]] <- skip

  args[["header"]] <- FALSE

  df <- do.call(data.table::fread, args)

  nms <- cols[args_select]

  names(df) <- nms

  classes <- file_vars[cols, "type"]

  classes <- classes[args_select]

  na_classes <- is.na(classes)

  classes <- ifelse(na_classes, "character", classes)

  sq_df <- seq_along(df)

  for (i in sq_df) {

    dfi <- df[[i]]

    class_i <- classes[[i]]

    dfi <- cast_to_type(dfi, class_i)

    dfi <- df[[i]]

  }

  attr(df, "file_vars") <- file_vars

  attr(df, "file_cols") <- cols

  df

}

#' @noRd
#' @importFrom utils read.delim unzip

rd_read <- function(fb_occurrence_obj) {

  file <- fb_occurrence_obj[["file"]]

  tsv <- fb_occurrence_obj[["tsv"]]

  n <- fb_occurrence_obj[["n"]]

  select <- fb_occurrence_obj[["select"]]

  keep_tsv <- fb_occurrence_obj[["keep_tsv"]]

  skip <- fb_occurrence_obj[["skip"]]

  quote <- ""

  use_unzip <- keep_tsv && !grepl("\\.tsv$", file)

  if (use_unzip) {

    unzip <- op_unzip()

    dir <- dirname(file)

    utils::unzip(file, tsv, exdir = dir, unzip = unzip)

  }

  connection_obj <- list(file = file, tsv = tsv, mode = "")

  con <- open_tsv_connection(connection_obj)

  df <- utils::read.delim(
    con, nrows = 1L, na.strings = "", quote = quote, skip = 0L
  )

  df_names <- names(df)

  cols <- fix_issue_vars(df_names)

  file_vars <- infer_file_vars(cols)

  select[["file_vars"]] <- file_vars

  lite <- attr(file_vars, "lite", TRUE)

  if (lite) {

    quote <- "\""

  }

  n_int <- as.integer(n)

  is_zero <- identical(n_int, 0L)

  if (is_zero) {

    df <- df[0L, ]

  } else {

    connection_obj <- list(file = file, tsv = tsv, mode = "")

    con <- open_tsv_connection(connection_obj)

    skip <- skip + 1L

    nrows <- abs(n)

    nrows <- max(nrows, 1L)

    sign <- sign(n)

    nrows <- nrows * sign

    df <- utils::read.delim(
      con,
      header = FALSE,
      quote = quote,
      na.strings = "",
      nrows = nrows,
      skip = skip
    )

    classes <- file_vars[cols, "type"]

    na_classes <- is.na(classes)

    classes <- ifelse(na_classes, "character", classes)

    sq_df <- seq_along(df)

    for (i in sq_df) {

      dfi <- df[[i]]

      class_i <- classes[[i]]

      dfi <- cast_to_type(dfi, class_i)

      dfi <- df[[i]]

    }

  }

  deselect <- deselect(select)

  idx <- !cols %in% deselect

  df <- df[idx]

  df_names <- cols[idx]

  names(df) <- df_names

  attr(df, "file_vars") <- file_vars

  df

}

#' @noRd

op_unzip <- function() {

  unzip <- "internal"

  option_unzip <- getOption("unzip")

  no_unzip <- is.null(unzip)

  no_unzip <- no_unzip || identical(unzip, "")

  if (!no_unzip) {

    unzip <- option_unzip

  }

  unzip

}

#' @noRd

deselect <- function(select) {

  file_vars <- select[["file_vars"]]

  type <- select[["type"]]

  file_vars_type <- file_vars[[type]]

  deselect <- select[["deselect"]]

  var_names <- var_names()

  deselect <- var_names[deselect, type]

  deselect <- file_vars_type %in% deselect

  deselect <- file_vars[deselect, ]

  row.names(deselect)

}

#' @noRd

select_facts <- function(select) {

  lite <- select[["lite"]]

  if (lite) {

    select[["facts"]] <- NULL

  }

  select

}

#' @noRd

spread_facts <-  function(facts) {

  select <- attr(facts, "facts", TRUE)

  type <- attr(facts, "fact_type", TRUE)

  id <- attr(facts, "id", TRUE)

  type_convert_facts <- attr(facts, "type_convert_facts", TRUE)

  drop_facts_na <- attr(facts, "drop_facts_na", TRUE)

  is_error <- inherits(facts, "try-error")

  if (is_error) {

    facts <- data.frame(
      Parent = NA_character_,
      Fact = NA_character_,
      Value = NA_character_,
      IntValue = NA_character_,
      DecimalValue = NA_character_
    )

  }

  missing_facts <- character()

  select_facts <- facts[["Fact"]]

  ind <- match(select, select_facts)

  fact_names <- names(facts)

  id_col <- fact_names == "Parent"

  fact_names[id_col] <- id

  names(facts) <- fact_names

  na_ind <- is.na(ind)

  any_na_ind <- any(na_ind)

  if (any_na_ind) {

    missing_facts <- select[na_ind]

    warning <- paste(missing_facts, collapse = ", ")

    warning(
      "Selected fact(s) - ",
      warning,
      " - could not be found in dataset",
      call. = FALSE
    )

    drop <- !isTRUE(drop_facts_na)

    missing_facts <- missing_facts[drop]

  }

  any_ind_not_na <- !all(na_ind)

  if (any_ind_not_na) {

    select <- select[!na_ind]

    ind <- select_facts %in% select

    ind <- which(ind)

    facts <- facts[ind, ]

    select_facts <- facts[["Fact"]]

    select_facts <- paste(type, "fact_", select_facts, sep = "_")

    facts[["Fact"]] <- select_facts

    fact_values <- facts[["Value"]]

    fact_ids <- c("Fact", id)

    fact_ids <- facts[fact_ids]

    facts <- tapply(fact_values, fact_ids, c, simplify = FALSE)

    fact_dimnames <- dimnames(facts)

    selected_fact_nms <- paste(type, "fact_", select, sep = "_")

    fact_nms <- fact_dimnames[["Fact"]]

    fact_nms <- intersect(selected_fact_nms, fact_nms)

    colnames <- c(id, fact_nms)

    ncols <- length(colnames)

    fact_list <- vector("list", ncols)

    names(fact_list) <- colnames

    ids <- fact_dimnames[[id]]

    fact_list[[id]] <- ids

    for (i in fact_nms) {

      fact_i <- facts[i, ]

      fact_i <- unname(fact_i)

      fact_null <- vapply(fact_i, is.null, NA)

      fact_i[fact_null] <- NA

      fact_i <- unlist_col(fact_i)

      if (type_convert_facts) {

        fact_i <- convert_col_type(fact_i)

      }

      fact_list[[i]] <- fact_i

    }

    row_names <- seq_along(ids)

    facts <- structure(fact_list, class = "data.frame", row.names = row_names)

  } else {

    facts <- facts[, id_col, drop = FALSE]

  }

  for (mf in missing_facts) {

    mfacts <- paste(type, "fact_", mf, sep = "_")

    facts[[mfacts]] <- NA_character_

  }

  attr(facts, "id") <- id

  unique(facts)

}

#' @noRd

bind_facts <- function(x) {

  facts <- attr(x, "facts_df", TRUE)

  id <- attr(facts, "id", TRUE)

  nms <- names(x)

  has_id <- id %in% nms

  stopifnot("Cannot bind facts. ID column missing from data" = has_id)

  attr <- attributes(x)

  facts_id <- facts[[id]]

  facts[[id]] <- NULL

  records_id <- x[[id]]

  matches <- match(records_id, facts_id)

  facts <- facts[matches, , drop = FALSE]

  x <- cbind(x, facts)

  attr[["names"]] <- names(x)

  attributes(x) <- attr

  x

}

#' @noRd

short_nms <- function(file_vars) {

  var_type <- attr(file_vars, "var_type", TRUE)

  short_nms <- file_vars[["shrtnm"]]

  short_nms <- c(short_nms, "abund", "crdUncert", "sciNm")

  translated_var <- c("abundance", "coordinates_uncertainty", "scientific_name")

  dwc <- c("individualCount", "coordinateUncertaintyInMeters", "scientificName")

  nms <- switch(
    var_type,
    translated_var = translated_var,
    dwc = dwc
  )

  file_vars_type <- file_vars[[var_type]]

  nms <- c(file_vars_type, nms)

  names(short_nms) <- nms

  short_nms

}

#' @noRd

unlist_col <- function(col) {

  col_unlisted <- unlist(col)

  col_len <- length(col)

  col_unlisted_len <- length(col_unlisted)

  len_eql <- identical(col_len, col_unlisted_len)

  if (len_eql) {

    col <- col_unlisted

  }

  col

}

#' @noRd

convert_col_type <- function(col) {

  is_list <- is.list(col)

  if (is_list) {

    col <- vapply(col, paste_col, "")

  }

  empty_col <- col == ""

  col[empty_col] <- NA_character_

  col_na <- is.na(col)

  col_nws <- trimws(col)

  col_nws_no_na <- col_nws[!col_na]

  is_num <- grepl("^[-+]?[0-9]*[\\.,]?[0-9]+([eE][-+]?[0-9]+)?$", col_nws_no_na)

  is_num <- all(is_num)

  if (is_num) {

    is_int <- !grepl("[\\.,]", col_nws_no_na)

    is_int <- all(is_int)

    col <- as.numeric(col_nws)

    if (is_int) {

      col <- as.integer(col_nws)

    }

  }

  col

}

#' @noRd

paste_col <- function(x) {

  is_na <- is.na(x)

  x[is_na] <- ""

  paste(x, collapse = ", ")

}

#' @noRd

infer_file_vars <- function(cols) {

  file_vars <- cite_file_vars()

  attr(file_vars, "lite") <- FALSE

  locale <- "none"

  n_cols <- length(cols)

  small_n_cols <- n_cols < 100L

  is_fact_df <- "Fact" %in% cols

  lite <- small_n_cols && !is_fact_df

  if (lite) {

    lite_download_file_vars <- lite_download_file_vars()

    file_vars <- lite_download_file_vars

    locale <- lapply(lite_download_file_vars, intersect, cols)

    locale <- vapply(locale, length, 0L)

    mx <- max(locale)

    locale <- locale == mx

    locale <- which(locale)

    locale_length <- length(locale)

    one_locale <- identical(locale_length, 1L)

    stopifnot(
      "Field names incompatible with this {finbif} package version" = one_locale
    )

    locale_nms <- names(locale)

    locale <- locale_nms[[1L]]

    file_vars_locale <- file_vars[[locale]]

    rownames(file_vars) <- file_vars_locale

    attr(file_vars, "lite") <- TRUE

  }

  attr(file_vars, "locale") <- locale

  file_vars

}

#' @noRd

preprocess_data_file <- function(file) {

  ext <- get_ext(file)

  switch(ext, .ods = from_ods(file), xlsx = from_xlsx(file), file)

}

#' @noRd

nlines <- function(fb_occurrence_obj) {

  file <- fb_occurrence_obj[["file"]]

  tsv <- fb_occurrence_obj[["tsv"]]

  connection_obj <- list(file = file, tsv = tsv, mode = "rb")

  con <- open_tsv_connection(connection_obj)

  on.exit({

    close(con)

  })

  n <- -1L

  cond <- TRUE

  while (cond) {

    chunk <- readBin(con, "raw", 65536L)

    raw10 <- as.raw(10L)

    chunk_10 <- chunk == raw10

    subtotal <- sum(chunk_10)

    n <- n + subtotal

    empty <- raw(0L)

    cond <- !identical(chunk, empty)

  }

  n

}

#' @noRd

open_tsv_connection <- function(connection_obj) {

  file <- connection_obj[["file"]]

  tsv <- connection_obj[["tsv"]]

  mode <- connection_obj[["mode"]]

  ext <- get_ext(file)

  switch(ext, .tsv = file(file, mode), unz(file, tsv, mode))

}

#' @noRd

get_ext <- function(file) {

  nchars <- nchar(file)

  start <- nchars - 3L

  substring(file, start, nchars)

}

#' @noRd

from_ods <- function(file) {

  has_read_ods <- has_pkgs("readODS")

  stopifnot("Package {readODS} required for ODS files" = has_read_ods)

  df <- readODS::read_ods(file, col_types = NA)

  write_tsv(df)

}

#' @noRd

from_xlsx <- function(file) {

  has_readxl <- has_pkgs("readxl")

  stopifnot("Package {readxl} required for Excel files" = has_readxl)

  df <- readxl::read_xlsx(
    file,
    progress = FALSE,
    col_types = "text",
    trim_ws = FALSE,
    .name_repair = "minimal"
  )

  write_tsv(df)

}

#' @noRd
#' @importFrom utils write.table

write_tsv <- function(df) {

  file <- tempfile(fileext = ".tsv")

  utils::write.table(
    df, file, quote = FALSE, sep = "\t", na = "", row.names = FALSE
  )

  file

}

#' @noRd

expand_lite_cols <- function(df) {

  select <- attr(df, "select", TRUE)

  add <- !select[["all"]]

  if (add) {

    formatted_taxon_name <- c(
      "scientific_name_interpreted",
      "common_name_english",
      "common_name_finnish",
      "common_name_swedish"
    )

    formatted_date_time <- c(
      "date_start",
      "date_end",
      "hour_start",
      "hour_end",
      "minute_start",
      "minute_end"
    )

    coordinates_euref <- c(
      "lat_min_euref",
      "lat_max_euref",
      "lon_min_euref",
      "lon_max_euref"
    )

    coordinates_1_ykj <- c("lon_1_ykj", "lat_1_ykj")

    coordinates_10_ykj <- c("lon_10_ykj", "lat_10_ykj")

    coordinates_1_center_ykj <- c("lon_1_center_ykj", "lat_1_center_ykj")

    coordinates_10_center_ykj <- c("lon_10_center_ykj", "lat_10_center_ykj")

    df_names <- names(df)

    file_vars <- attr(df, "file_vars", TRUE)

    locale <- attr(file_vars, "locale", TRUE)

    translated_vars <- file_vars[["translated_var"]]

    cols <- c(
      "formatted_taxon_name",
      "formatted_date_time",
      "coordinates_euref",
      "coordinates_1_ykj",
      "coordinates_10_ykj",
      "coordinates_1_center_ykj",
      "coordinates_10_center_ykj"
    )

    cols <- translated_vars %in% cols

    cols <- which(cols)

    for (col in cols) {

      col_nm <- file_vars[col, ]

      col_nm <- rownames(col_nm)

      has_col_nm <- col_nm %in% df_names

      df_col <- df[[col_nm]]

      df_col_na <- is.na(df_col)

      expand <- has_col_nm && !all(df_col_na)

      if (expand) {

        attr(df_col, "locale") <- locale

        translated_var <- file_vars[[col, "translated_var"]]

        type <- switch(
          translated_var,
          formatted_taxon_name = "taxon",
          formatted_date_time = "date_time",
          coordinates_euref = "coordinates_euref",
          "coords"
        )

        split_cols <- switch(
          type,
          taxon = split_taxa_col(df_col),
          date_time = split_dt_col(df_col),
          coordinates_euref = split_coord_euref_col(df_col),
          coords = split_coord_col(df_col),
        )

        new_cols <- switch(
          translated_var,
          formatted_taxon_name = formatted_taxon_name,
          formatted_date_time = formatted_date_time,
          coordinates_euref = coordinates_euref,
          coordinates_1_ykj = coordinates_1_ykj,
          coordinates_10_ykj = coordinates_10_ykj,
          coordinates_1_center_ykj = coordinates_1_center_ykj,
          coordinates_10_center_ykj = coordinates_10_center_ykj
        )

        new_cols <- translated_vars %in% new_cols

        new_cols <- file_vars[new_cols, ]

        new_cols <- rownames(new_cols)

        sq <- seq_along(new_cols)

        for (i in sq) {

          col <- new_cols[[i]]

          dfi <- df[[col]]

          no_col <- is.null(dfi)

          na_dfi <- is.na(dfi)

          no_col <- no_col || all(na_dfi)

          if (no_col) {

             dfi <- split_cols[[i]]

             df[[col]] <- dfi

          }

        }

      }

    }

  }

  df

}

#' @noRd

split_taxa_col <- function(col) {

  locale <- attr(col, "locale", TRUE)

  col <- list(col, n = 2L, split = " \u2014 ")

  split_cols <- split_col(col)

  col1 <- split_cols[[1L]]

  col2 <- split_cols[[2L]]

  col2 <- list(col2, n = 2L, split = " \\(|\\)")

  common_names <- split_col(col2)

  common_names1 <- common_names[[1L]]

  common_names2 <- common_names[[2L]]

  split_cols <- list(scientific_name = col1)

  common_names_na <- is.na(common_names1)

  common_names1 <- ifelse(common_names_na, locale, common_names1)

  locales <- c("en", "fi", "sv")

  for (loc in locales) {

    ind <- common_names1 == loc

    col_loc <- NA_character_

    col_loc_values <- common_names2[ind]

    col_loc[ind] <- col_loc_values

    split_cols[[loc]] <- col_loc

  }

  split_cols

}

#' @noRd

split_dt_col <- function(col) {

  col <- list(col, n = 2L, split = " - ")

  col <- split_col(col)

  col <- lapply(col, list, n = 2L, split = " \\[|\\]")

  col <- lapply(col, split_col)

  dates <- lapply(col, "[[", 2L)

  dates1 <- dates[[1L]]

  dates2 <- dates[[2L]]

  dates_na <- is.na(dates1)

  dates1 <- ifelse(dates_na, dates2, dates1)

  times <- lapply(col, "[[", 1L)

  times <- lapply(times, list, n = 2L, split = "-")

  times <- lapply(times, split_col)

  times1 <- times[[1L]]

  times1 <- times1[[1L]]

  times2 <- times[[2L]]

  times2_1 <- times2[[1L]]

  start_times <- times2[[2L]]

  start_times <- list(start_times, n = 2L, split = ":")

  start_times <- split_col(start_times)

  minute_start <- start_times[[1L]]

  minute_start <- as.integer(minute_start)

  hour_start <- start_times[[2L]]

  hour_start <- as.integer(hour_start)

  times_na <- is.na(times1)

  end_times <- ifelse(times_na, times2_1, times1)

  end_times <- list(end_times, n = 2L, split = ":")

  end_times <- split_col(end_times)

  minute_end <- end_times[[1L]]

  minute_end <- as.integer(minute_end)

  hour_end <- end_times[[2L]]

  hour_end <- as.integer(hour_end)

  list(
    date_start = dates2,
    date_end = dates1,
    hour_start = hour_start,
    hour_end =  hour_end,
    minute_start = minute_start,
    minute_end = minute_end
  )

}

#' @noRd

split_coord_col <- function(col) {

  col <- list(col, n = 2L, split = ":")

  split_cols <- split_col(col)

  lapply(split_cols, as.numeric)

}

#' @noRd

split_coord_euref_col <- function(col) {

  col <- list(col, n = 4L, split = "\\D")

  split_cols <- split_col(col)

  split_cols <- lapply(split_cols, as.numeric)

  rev(split_cols)

}

#' @noRd

split_col <- function(split_obj) {

  col <- split_obj[[1L]]

  col <- as.character(col)

  n <- split_obj[["n"]]

  sq <- seq_len(n)

  split <- split_obj[["split"]]

  split_cols <- strsplit(col, split)

  split_cols <- lapply(split_cols, c, NA_character_)

  split_cols <- lapply(split_cols, "[", sq)

  split_cols <- lapply(split_cols, rev)

  split_cols <- do.call(rbind, split_cols)

  apply(split_cols, 2L, c, simplify = FALSE)

}
