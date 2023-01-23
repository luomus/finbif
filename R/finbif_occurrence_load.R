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
#'   return value. Note that this functionality requires that the `{dplyr}` and
#'   `{tidyr}` packages are available.
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
#' @importFrom digest digest
#' @importFrom httr progress RETRY status_code write_disk
#' @importFrom utils hasName head read.delim tail unzip write.table
#' @export

finbif_occurrence_load <- function(
  file, select = NULL, n = -1, count_only = FALSE,
  quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"), dwc = FALSE, date_time_method = NULL,
  tzone = getOption("finbif_tz"), write_file = tempfile(), dt = NA,
  keep_tsv = FALSE, facts = list(), type_convert_facts = TRUE, drop_na = FALSE,
  drop_facts_na = drop_na, locale = getOption("finbif_locale"), skip = 0
) {

  fb_records_obj <- list(
    file = preprocess_data_file(file),
    n = as.integer(n),
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

  select_all <- FALSE
  short <- FALSE
  deselect <- character()

  if (any(c("all", "short") %in% select)) {

    short <- identical(select[[1L]], "short")

    deselect <- grep("^-", select, value = TRUE)
    deselect <- gsub("-", "", deselect)
    deselect <- match(deselect, var_names[, var_type])
    deselect <- row.names(var_names[deselect, ])

    select <- "default_vars"
    select_all <- TRUE

  }

  defer_errors({

    fb_records_obj[["aggregate"]] <- "none"

    fb_records_obj[["select"]] <- select

    fb_records_obj[["include_facts"]] <- FALSE

    fb_records_obj[["var_type"]] <- var_type

    select <- infer_selection(fb_records_obj)

  })

  fact_types <- names(which(vapply(facts, length, integer(1L)) > 0L))

  select[["deselect"]] <- deselect
  select[["all"]] <- select_all
  select[["type"]] <- var_type
  select[["facts"]] <- fact_types

  fb_records_obj[["select"]] <- select

  fb_occurrence_df <- read_finbif_tsv(fb_records_obj)

  if (count_only) {

    return(fb_occurrence_df)

  }

  file_vars <- attr(fb_occurrence_df, "file_vars")

  attr(file_vars, "var_type") <- var_type

  select[["lite"]] <- attr(file_vars, "lite")

  attr(fb_occurrence_df, "locale") <- locale

  fb_occurrence_df <- localise_enums(fb_occurrence_df)

  n_recs <- attr(fb_occurrence_df, "nrow")

  url <- attr(fb_occurrence_df, "url")

  names(fb_occurrence_df) <- fix_issue_vars(names(fb_occurrence_df))

  select <- select_facts(select)

  attr(fb_occurrence_df, "select") <- select

  fb_occurrence_df <- new_vars(fb_occurrence_df)

  record_id <- file_vars[["translated_var"]] == "record_id"

  record_id <- rownames(file_vars[record_id, ])

  record_id <- fb_occurrence_df[[record_id]]

  fb_occurrence_df <- expand_lite_cols(fb_occurrence_df)

  nms <- file_vars[names(fb_occurrence_df), var_type]

  names(fb_occurrence_df) <- ifelse(is.na(nms), names(fb_occurrence_df), nms)

  fb_occurrence_df <- structure(
    fb_occurrence_df,
    select_user = select[["user"]],
    column_names = select[["user"]],
    aggregate = "none",
    dwc = dwc,
    date_time_method = date_time_method,
    tzone = tzone,
    locale = locale,
    include_new_cols = !select[["all"]],
    record_id = record_id
  )

  fb_occurrence_df <- compute_vars_from_id(fb_occurrence_df)

  fb_occurrence_df <- compute_abundance(fb_occurrence_df)

  fb_occurrence_df <- compute_citation(fb_occurrence_df)

  fb_occurrence_df <- compute_coordinate_uncertainty(fb_occurrence_df)

  fb_occurrence_df <- compute_scientific_name(fb_occurrence_df)

  fb_occurrence_df <- add_nas(fb_occurrence_df)

  if (select[["all"]]) {

    select[["user"]] <- names(fb_occurrence_df)

  } else {

    date_time_method <- det_datetime_method(date_time_method, n_recs)

    fb_occurrence_df <- structure(
      fb_occurrence_df,
      select_user = select[["user"]],
      column_names = select[["user"]],
      aggregate = "none",
      dwc = dwc,
      date_time = select[["date_time_selected"]],
      date_time_method = date_time_method,
      tzone = tzone
    )

    fb_occurrence_df <- date_times(fb_occurrence_df)

    fb_occurrence_df <- compute_date_time(fb_occurrence_df)

    fb_occurrence_df <- compute_duration(fb_occurrence_df)

    fb_occurrence_df <- compute_iso8601(fb_occurrence_df)

    fb_occurrence_df <- any_issues(fb_occurrence_df)

    for (extra_var in setdiff(select[["user"]], names(fb_occurrence_df))) {

      ind <- var_names[[var_type]] == extra_var

      fb_occurrence_df[[extra_var]] <- cast_to_type(
        rep_len(NA, nrow(fb_occurrence_df)), var_names[ind, "type"]
      )

    }

  }

  attr(fb_occurrence_df, "file_cols") <- NULL
  attr(fb_occurrence_df, "file_vars") <- NULL

  fb_occurrence_df <- structure(
    fb_occurrence_df,
    class     = c("finbif_occ", class(fb_occurrence_df)),
    nrec_dnld = n_recs,
    nrec_avl  = n_recs,
    url       = url,
    time      = "??",
    short_nms = short,
    dwc       = dwc && !short,
    record_id = record_id
  )

  for (fact_type in fact_types) {

    stopifnot(
      "Invalid fact type" = fact_type %in% c("record", "event", "document")
    )

    fb_records_obj[["select"]] <- list(
      all = TRUE, deselect = character(), type = "translated_var"
    )

    fb_records_obj[["n"]] <- -1L

    fb_records_obj[["facts"]] <- fact_type

    facts_df <- try(read_finbif_tsv(fb_records_obj), silent = TRUE)

    id <- switch(
      fact_type,
      record = file_vars[["Unit.UnitID", var_type]],
      event = file_vars[["Gathering.GatheringID", var_type]],
      document = file_vars[["Document.DocumentID", var_type]]
    )

    facts_df <- structure(
      facts_df,
      facts = facts[[fact_type]],
      fact_type = fact_type,
      id = id,
      type_convert_facts = type_convert_facts,
      drop_facts_na = drop_facts_na
    )

    facts_df <- spread_facts(facts_df)

    select[["user"]] <- c(
      names(fb_occurrence_df[select[["user"]]]), setdiff(names(facts_df), id)
    )

    attr(fb_occurrence_df, "facts_df") <- facts_df

    fb_occurrence_df <- bind_facts(fb_occurrence_df)

  }

  if (short) {

    short_nms <- short_nms(file_vars)[names(fb_occurrence_df)]

    short_fcts <- grep("_fact__", names(fb_occurrence_df), value = TRUE)

    short_fcts <- sub("^.*_fact__", "", short_fcts)

    short_fcts <- sub("http://tun.fi/", "", short_fcts)

    short_fcts <- abbreviate(
      short_fcts, 9 - ceiling(log10(length(short_fcts) + .1)), FALSE,
      strict = TRUE, method = "both.sides"
    )
    short_fcts <- paste0("f", seq_along(short_fcts), short_fcts)

    short_nms[is.na(short_nms)] <- short_fcts

    missing <- which(short_nms == "f")

    short_nms[missing] <- abbreviate(
      gsub("[^A-Za-z]", "", names(fb_occurrence_df)[missing]), 10L
    )

    names(fb_occurrence_df) <- short_nms

    select[["user"]] <- names(fb_occurrence_df)

  }

  select[["user"]] <- name_chr_vec(select[["user"]])

  fb_occurrence_df <- fb_occurrence_df[, select[["user"]], drop = FALSE]

  names(fb_occurrence_df) <- names(select[["user"]])

  attr(fb_occurrence_df, "column_names") <- select[["user"]]

  attr(fb_occurrence_df, "drop_na") <- drop_na

  drop_na_col(fb_occurrence_df)

}

#' @noRd
read_finbif_tsv <- function(fb_occurrenc_obj) {

  file <- as.character(fb_occurrenc_obj[["file"]])

  fb_occurrenc_obj[["file"]] <- file

  n <- fb_occurrenc_obj[["n"]]

  count_only <- fb_occurrenc_obj[["count_only"]]

  facts <- fb_occurrenc_obj[["facts"]]

  ptrn <- "^https?://.+?/HBF\\."

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

  valid_facts <- facts %in% c("none", "record", "event", "document")

  stopifnot(
    "Facts can only be of types: record, event and/or document" = valid_facts
  )

  file <- gsub("rows_", tsv_prefix, file)

  fb_occurrenc_obj[["file"]] <- file

  tsv <- paste0(tsv_prefix, tsv)

  fb_occurrenc_obj[["tsv"]] <- tsv

  if (grepl("^[0-9]*$", file)) {

    url <- sprintf("%s/HBF.%s", getOption("finbif_dl_url"), file)

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

  if (identical(n, -1L)) {

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

  if (is.na(dt)) {

    use_dt <- TRUE

    dt <- FALSE

    fb_occurrence_obj[["dt"]] <- dt

  } else {

    use_dt <- dt

  }

  all <- identical(n, -1L)

  if (count_only) {

    nlines(file, tsv)

  } else {

    n_rows <- NULL

    if (!all) {

      n_rows <- nlines(file, tsv)

    }

    if (has_pkgs("data.table") && use_dt) {

      input <- as.character(file)

      if (grepl("\\.tsv$", input)) {

        fb_occurrence_obj[["keep_tsv"]] <- FALSE

        fb_occurrence_obj[["dt_args"]] <- list(input = input)

        df <- dt_read(fb_occurrence_obj)

      } else {

        fb_occurrence_obj[["dt_args"]] <- list(
          zip = list(input = input, tsv = tsv)
        )

        df <- dt_read(fb_occurrence_obj)

      }

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

  locale <- attr(df, "locale", TRUE)

  for (i in names(df)) {

    if (i %in% row.names(file_vars) && isTRUE(file_vars[[i, "localised"]])) {

      labels_obj <- list(
        labels = df[[i]], col = i, var_names = file_vars, locale =  locale
      )

      df[[i]] <- localise_labels(labels_obj)

    }

  }

  df

}

#' @noRd
fix_issue_vars <- function(x) {

  type <- c("Time", "Location")

  for (i in c("Issue", "Source", "Message")) {

    for (j in 1:2) {

      x <- gsub(
        sprintf("Issue.%s.%s", i, j), sprintf("%sIssue.%s", type[j], i), x
      )

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

  if (is.null(attr(df, "file_cols"))) {

    attr(df, "file_cols") <- names(df)

  }

  nms_df <- attr(df, "file_cols")

  ind <- file_vars[["superseeded"]] == "FALSE"

  ss <- rownames(file_vars[!ind, ])
  ss <- intersect(ss, nms_df)
  ss <- file_vars[ss, "superseeded"]

  deselect <- var_names[deselect, "translated_var"]
  deselect <- file_vars[["translated_var"]] %in% deselect

  ind <- ind & !deselect

  nms <- row.names(file_vars[ind, ])

  new_vars <- setdiff(nms, c(nms_df, ss))

  if (add) {

    for (i in new_vars) {

      df[[i]] <- rep_len(NA, nrow(df))

    }

  }

  df

}

#' @noRd
get_zip <- function(fb_occurrenc_obj) {

  url <- fb_occurrenc_obj[["url"]]

  quiet <- fb_occurrenc_obj[["quiet"]]

  cache <- fb_occurrenc_obj[["cache"]]

  write_file <- fb_occurrenc_obj[["write_file"]]

  if (cache) {

    hash <- digest::digest(sub(":\\d+", "", url))

    fcp <- getOption("finbif_cache_path")

    if (is.null(fcp)) {

      cache_file <- get_cache(hash)

      if (!is.null(cache_file)) {

        fb_occurrenc_obj[["file"]] <- cache_file

        return(fb_occurrenc_obj)

      }

      on.exit({

        if (!is.null(write_file)) {

          set_cache(list(data = write_file, hash = hash))

        }

      })

    } else {

      write_file <- file.path(fcp, paste0("finbif_cache_file_", hash))

      if (file.exists(write_file)) {

        fb_occurrenc_obj[["file"]] <- write_file

        return(fb_occurrenc_obj)

      }

    }

  }

  progress <- NULL

  if (!quiet) {

    progress <- httr::progress()

  }

  stopifnot(
    "Request not cached and option:finbif_allow_query = FALSE" =
      getOption("finbif_allow_query")
  )

  Sys.sleep(1 / getOption("finbif_rate_limit"))

  query <- list()

  auth <- Sys.getenv("FINBIF_RESTRICTED_FILE_ACCESS_TOKEN")

  if (!identical(auth, "")) {

    query <- list(personToken = auth)

  }

  resp <- httr::RETRY(
    "GET",
    url,
    httr::write_disk(write_file, overwrite = TRUE),
    progress,
    query = query,
    times = getOption("finbif_retry_times"),
    pause_base = getOption("finbif_retry_pause_base"),
    pause_cap = getOption("finbif_retry_pause_cap"),
    pause_min = getOption("finbif_retry_pause_min"),
    quiet = quiet,
    terminate_on = 404L
  )

  fs <- file.size(write_file)

  fl <- Sys.getenv("FINBIF_FILE_SIZE_LIMIT")

  fl <- as.integer(fl)

  if (isTRUE(fs > fl)) {

    stop("File download too large; err_name: too_large", call. = FALSE)

  }

  if (!quiet) message("")

  code <- httr::status_code(resp)

  if (!identical(code, 200L)) {

    stop(
      sprintf("File request failed [%s]; err_name: request_failed", code),
      call. = FALSE
    )

  }

  fb_occurrenc_obj[["file"]] <- write_file

  fb_occurrenc_obj

}

#' @noRd
add_nas <- function(df) {

  dwc <- attr(df, "dwc", TRUE)

  var_type <- col_type_string(dwc)

  file_vars <- attr(df, "file_vars", TRUE)

  for (nm in names(df)) {

    ans <- df[[nm]]

    if (all(is.na(ans))) {

      ind <- file_vars[[var_type]] == nm

      l <- length(which(ind))

      if (l > 1L) {

        ind <- ind & file_vars[["superseeded"]] == "FALSE"

      }

      if (l < 1L) {

        file_vars <- var_names

        ind <- file_vars[[var_type]] == nm

      }

      df[[nm]] <- cast_to_type(ans, file_vars[ind, "type"])

    }

  }

  df

}

#' @noRd
any_issues <- function(fb_occurrence_df) {

  df <- fb_occurrence_df

  dwc <- attr(fb_occurrence_df, "dwc", TRUE)

  var_type <- col_type_string(dwc)

  vnms <- var_names[var_type]
  any_issue <- vnms["unit.quality.documentGatheringUnitQualityIssues", ]

  select_user <- attr(fb_occurrence_df, "select_user", TRUE)

  if (!utils::hasName(df, any_issue) && any_issue %in% select_user) {

    rec_iss <- !is.na(df[[vnms["unit.quality.issue.issue", ]]])
    ev_iss  <- !is.na(df[[vnms["gathering.quality.issue.issue", ]]])
    tm_iss  <- !is.na(df[[vnms["gathering.quality.timeIssue.issue", ]]])
    loc_iss <- !is.na(df[[vnms["gathering.quality.locationIssue.issue", ]]])

    iss <- rec_iss | ev_iss | tm_iss | loc_iss

    if (length(iss) < 1L && nrow(df) > 0L) iss <- NA

    df[[any_issue]] <- iss

  }

  df

}

#' @noRd
dt_read <- function(fb_occurrence_obj) {

  select <- fb_occurrence_obj[["select"]]

  n <- fb_occurrence_obj[["n"]]

  quiet <- fb_occurrence_obj[["quiet"]]

  dt <- fb_occurrence_obj[["dt"]]

  keep_tsv <- fb_occurrence_obj[["keep_tsv"]]

  skip <- fb_occurrence_obj[["skip"]]

  args <- list(
    nrows = 0, showProgress = !quiet, data.table = dt, na.strings = "",
    quote = "", sep = "\t", fill = TRUE, check.names = FALSE, header = TRUE,
    skip = 0L
  )

  args <- c(fb_occurrence_obj[["dt_args"]], args)

  if (utils::hasName(args, "zip")) {

    unzip <- "internal"

    if (!is.null(getOption("unzip")) && !identical(getOption("unzip"), "")) {

      unzip <- getOption("unzip")

    }

    args[["input"]] <- sprintf(
      "%s/%s", dirname(args[["zip"]][["input"]]), args[["zip"]][["tsv"]]
    )

    if (!file.exists(args[["input"]])) {

      utils::unzip(
        args[["zip"]][["input"]], files = args[["zip"]][["tsv"]],
        exdir = dirname(args[["zip"]][["input"]]), unzip = unzip
      )

      if (!keep_tsv) {

        on.exit(unlink(args[["input"]]))

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

  if (attr(file_vars, "lite")) {

    args[["quote"]] <- "\""

  }

  if (select[["all"]]) {

    args[["select"]] <- which(!cols %in% deselect(select))

  } else {

    iss <- "unit.quality.documentGatheringUnitQualityIssues"
    iss <- iss %in% select[["query"]]

    if (iss) {
      select[["query"]] <- c(
        select[["query"]],
        "unit.quality.issue.issue",
        "gathering.quality.issue.issue",
        "gathering.quality.timeIssue.issue",
        "gathering.quality.locationIssue.issue"
      )
    }

    select_vars <-  var_names[select[["query"]], select[["type"]]]

    select_vars <- file_vars[[select[["type"]]]] %in% select_vars

    expand_vars <- c(
      "formatted_taxon_name", "formatted_date_time",
      "coordinates_euref", "coordinates_1_ykj", "coordinates_10_ykj",
      "coordinates_1_center_ykj", "coordinates_10_center_ykj"
    )

    expand_vars <- file_vars[["translated_var"]] %in% expand_vars

    select_vars <- switch(
      attr(file_vars, "locale"),
      none = select_vars,
      select_vars | expand_vars
    )

    select_vars <- row.names(file_vars[select_vars, ])

    args[["select"]] <- which(cols %in% select_vars)

    for (ftype in select[["facts"]]) {

      id_col <- switch(
        ftype,
        record = "Unit.UnitID",
        event = "Gathering.GatheringID",
        document = "Document.DocumentID"
      )

      id_col <- which(cols %in% id_col)

      args[["select"]]  <- c(args[["select"]], id_col)

    }

    args[["select"]] <- sort(unique(args[["select"]]))

  }

  args[["nrows"]] <- as.double(n)
  args[["check.names"]] <- TRUE
  args[["skip"]] <- skip + 1L
  args[["header"]] <- FALSE

  df <- do.call(data.table::fread, args)

  names(df) <- cols[args[["select"]]]

  classes <- file_vars[cols, "type"]

  classes <- classes[args[["select"]]]

  classes <- ifelse(is.na(classes), "character", classes)

  for (i in seq_along(df)) {

    df[[i]] <- cast_to_type(df[[i]], classes[[i]])

  }

  attr(df, "file_vars") <- file_vars

  attr(df, "file_cols") <- cols

  df

}

#' @noRd
rd_read <- function(fb_occurrence_obj) {

  file <- fb_occurrence_obj[["file"]]

  tsv <- fb_occurrence_obj[["tsv"]]

  n <- fb_occurrence_obj[["n"]]

  select <- fb_occurrence_obj[["select"]]

  keep_tsv <- fb_occurrence_obj[["keep_tsv"]]

  skip <- fb_occurrence_obj[["skip"]]

  quote <- ""

  if (keep_tsv && !grepl("\\.tsv$", file)) {

    unzip <- "internal"

    if (!is.null(getOption("unzip")) && !identical(getOption("unzip"), "")) {

      unzip <- getOption("unzip")

    }

    utils::unzip(file, tsv, exdir = dirname(file), unzip = unzip)

  }

  con <- open_tsv_connection(file, tsv, "")

  df <- utils::read.delim(
    con, nrows = 1L, na.strings = "", quote = quote, skip = 0L
  )

  cols <- fix_issue_vars(names(df))

  file_vars <- infer_file_vars(cols)

  select[["file_vars"]] <- file_vars

  if (attr(file_vars, "lite")) {

    quote <- "\""

  }

  if (identical(as.integer(n), 0L)) {

    df <- df[0L, ]

  } else {

    con <- open_tsv_connection(file, tsv, "")

    df <- utils::read.delim(
      con, header = FALSE, quote = quote, na.strings = "",
      nrows = max(abs(n), 1L) * sign(n), skip = skip + 1L
    )

    classes <- file_vars[cols, "type"]

    classes <- ifelse(is.na(classes), "character", classes)

    for (i in seq_along(df)) {

      df[[i]] <- cast_to_type(df[[i]], classes[[i]])

    }

  }

  idx <- !cols %in% deselect(select)

  df <- df[idx]

  names(df) <- cols[idx]

  attr(df, "file_vars") <- file_vars

  df

}

#' @noRd
deselect <- function(select) {

  file_vars <- select[["file_vars"]]
  deselect <- var_names[select[["deselect"]], select[["type"]]]
  deselect <- file_vars[[select[["type"]]]] %in% deselect
  row.names(file_vars[deselect, ])

}

#' @noRd
select_facts <- function(select) {

  if (select[["lite"]]) {

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

  if (inherits(facts, "try-error")) {

    facts <- data.frame(
      Parent = NA_character_,
      Fact = NA_character_,
      Value = NA_character_,
      IntValue = NA_character_,
      DecimalValue = NA_character_
    )

  }

  missing_facts <- character()

  ind <- match(select, facts[[2L]])

  id_col <- names(facts) == "Parent"

  names(facts)[id_col] <- id

  if (anyNA(ind)) {

    missing_facts <- select[is.na(ind)]

    warning(
      "Selected fact(s) - ", paste(missing_facts, collapse = ", "),
      " - could not be found in dataset", call. = FALSE
    )

    missing_facts <- missing_facts[!isTRUE(drop_facts_na)]

  }

  if (!all(is.na(ind))) {

    select <- select[!is.na(ind)]

    ind <- which(facts[[2L]] %in% select)

    facts <- facts[ind, ]

    facts[[2L]] <- paste(type, "fact_", facts[[2L]], sep = "_")

    stopifnot(
      "Package {tidyr} is required for this functionality" = has_pkgs("tidyr")
    )

    facts <- tidyr::pivot_wider(
      facts, 1L, names_from = 2L, values_from = 3L, values_fn = list,
      values_fill = list(NA)
    )

    fact_cols <- names(facts) != id

    facts[fact_cols] <- lapply(facts[fact_cols], unlist_col)

    fact_cols[fact_cols] <- rep_len(type_convert_facts, length(select))

    facts[fact_cols] <- lapply(facts[fact_cols], convert_col_type)

    facts <- as.data.frame(facts)

  } else {

    facts <- facts[, id_col, drop = FALSE]

  }

  for (mf in missing_facts) {

    facts[[paste(type, "fact_", mf, sep = "_")]] <- NA_character_

  }

  attr(facts, "id") <- id

  unique(facts)

}

#' @noRd
bind_facts <- function(x) {

  stopifnot(
    "Package {dplyr} is required for this functionality" = has_pkgs("dplyr")
  )

  facts <- attr(x, "facts_df", TRUE)

  id <- attr(facts, "id", TRUE)

  stopifnot(
    "Cannot bind facts. ID column missing from data" = utils::hasName(x, id)
  )

  attr <- attributes(x)

  x <- dplyr::left_join(x, facts, by = id)

  attr[["names"]] <- names(x)

  attributes(x) <- attr

  x

}

#' @noRd
short_nms <- function(file_vars) {

  var_type <- attr(file_vars, "var_type", TRUE)

  short_nms <- c(file_vars[["shrtnm"]], "abund", "crdUncert", "sciNm")

  nms <- switch(
    var_type,
    translated_var = c(
      "abundance", "coordinates_uncertainty", "scientific_name"
    ),
    dwc = c(
      "individualCount", "coordinateUncertaintyInMeters", "scientificName"
    )
  )

  names(short_nms) <- c(file_vars[[var_type]], nms)

  short_nms

}

#' @noRd
unlist_col <- function(col) {

  col_ <- unlist(col)

  l1 <- length(col)

  l2 <- length(col_)

  cond <- identical(l1, l2)

  if (cond) col <- col_

  col

}

#' @noRd
convert_col_type <- function(col) {

  if (is.list(col)) {

    col <- vapply(col, paste_col, character(1L))

  }

  col[col == ""] <- NA_character_

  col_na <- is.na(col)

  col_nws <- trimws(col)

  is_num <- grepl(
    "^[-+]?[0-9]*[\\.,]?[0-9]+([eE][-+]?[0-9]+)?$", col_nws[!col_na]
  )

  is_num <- all(is_num)

  if (is_num) {

    is_int <- !grepl("[\\.,]", col_nws[!col_na])

    is_int <- all(is_int)

    if (is_int) {

      col <- as.integer(col_nws)

    } else {

      col <- as.numeric(col_nws)

    }

  }

  col

}

#' @noRd
paste_col <- function(x) {

  x[is.na(x)] <- ""

  paste(x, collapse = ", ")

}

#' @noRd
infer_file_vars <- function(cols) {

  file_vars <- cite_file_vars

  attr(file_vars, "lite") <- FALSE

  locale <- "none"

  if (length(cols) < 100L && !"Fact" %in% cols) {

    file_vars <- lite_download_file_vars

    locale <- lapply(lite_download_file_vars, intersect, cols)
    locale <- vapply(locale, length, integer(1L))
    locale <- which(locale == max(locale))

    stopifnot(
      "File has field names incompatible with this {finbif} R package version" =
        length(locale) == 1L
     )

    locale <- names(locale)[[1L]]

    rownames(file_vars) <- file_vars[[locale]]

    attr(file_vars, "lite") <- TRUE

  }

  attr(file_vars, "locale") <- locale

  file_vars

}

#' @noRd
preprocess_data_file <- function(file) {

  if (grepl("\\.ods$", file)) {

    file <- from_ods(file)

  }

  if (grepl("\\.xlsx$", file)) {

    file <- from_xlsx(file)

  }

  file

}

#' @noRd
from_ods <- function(file) {

  stopifnot("Package {readODS} required for ODS files" = has_pkgs("readODS"))

  df <- readODS::read_ods(file, col_types = NA)

  write_tsv(df)

}

#' @noRd
from_xlsx <- function(file) {

  stopifnot("Package {readxl} required for Excel files" = has_pkgs("readxl"))

  df <- readxl::read_xlsx(
    file, progress = FALSE, col_types = "text", trim_ws = FALSE,
    .name_repair = "minimal"
  )

  write_tsv(df)

}

#' @noRd
write_tsv <- function(df) {

  file <- tempfile(fileext = ".tsv")

  write.table(df, file, quote = FALSE, sep = "\t", na = "", row.names = FALSE)

  file

}

#' @noRd
expand_lite_cols <- function(df) {

  select <- attr(df, "select", TRUE)

  add <- !select[["all"]]

  file_vars <- attr(df, "file_vars")

  cols <- c(
    "formatted_taxon_name", "formatted_date_time",
    "coordinates_euref", "coordinates_1_ykj", "coordinates_10_ykj",
    "coordinates_1_center_ykj", "coordinates_10_center_ykj"
  )

  cols <- which(file_vars[["translated_var"]] %in% cols[add])

  for (col in cols) {

    col_nm <- rownames(file_vars[col, ])

    type <- switch(
      file_vars[[col, "translated_var"]],
      formatted_taxon_name = "taxon",
      formatted_date_time = "date_time",
      coordinates_euref = "coordinates_euref",
      "coords"
    )

    if (utils::hasName(df, col_nm) && !all(is.na(df[[col_nm]]))) {

      df_col <- df[[col_nm]]

      attr(df_col, "locale") <- attr(file_vars, "locale", TRUE)

      split_cols <- switch(
        type,
        taxon = split_taxa_col(df_col),
        date_time = split_dt_col(df_col),
        coordinates_euref = split_coord_euref_col(df_col),
        coords = split_coord_col(df_col),
      )

      new_cols <- switch(
        file_vars[[col, "translated_var"]],
        formatted_taxon_name = c(
          "scientific_name_interpreted", "common_name_english",
          "common_name_finnish", "common_name_swedish"
        ),
        formatted_date_time = c(
          "date_start", "date_end", "hour_start", "hour_end",
          "minute_start", "minute_end"
        ),
        coordinates_euref = c(
          "lat_min_euref", "lat_max_euref", "lon_min_euref", "lon_max_euref"
        ),
        coordinates_1_ykj = c("lon_1_ykj", "lat_1_ykj"),
        coordinates_10_ykj = c("lon_10_ykj", "lat_10_ykj"),
        coordinates_1_center_ykj = c("lon_1_center_ykj", "lat_1_center_ykj"),
        coordinates_10_center_ykj = c("lon_10_center_ykj", "lat_10_center_ykj")
      )

      new_cols <- file_vars[["translated_var"]] %in% new_cols
      new_cols <- rownames(file_vars[new_cols, ])

      for (i in seq_along(new_cols)) {

        cond <- is.null(df[[new_cols[[i]]]])
        cond <- cond || all(is.na(df[[new_cols[[i]]]]))

        if (cond) {

          df[[new_cols[[i]]]] <- split_cols[[i]]

        }

      }

    }

  }

  df

}

#' @noRd
split_taxa_col <- function(col) {

  locale <- attr(col, "locale", TRUE)

  split_cols <- split_col(col, " \u2014 ")

  common_names <- split_col(split_cols[[2L]], " \\(|\\)")

  split_cols <- list(scientific_name = split_cols[[1L]])

  common_names[[1L]] <- ifelse(
    is.na(common_names[[1L]]), locale, common_names[[1L]]
  )

  locales <- c("en", "fi", "sv")

  for (l in locales) {

    ind <- common_names[[1L]] == l
    split_cols[[l]] <- NA_character_
    split_cols[[l]][ind] <- common_names[[2L]][ind]

  }

  split_cols

}

#' @noRd
split_dt_col <- function(col) {

  split_cols <- split_col(col, " - ")
  split_cols <- lapply(split_cols, split_col, " \\[|\\]")

  dates <- lapply(split_cols, utils::tail, 1L)
  dates <- lapply(dates, unlist)

  dates[[1L]] <- ifelse(is.na(dates[[1L]]), dates[[2L]], dates[[1L]])

  times <- lapply(split_cols, utils::head, 1L)
  times <- lapply(times, unlist)
  times <- lapply(times, split_col, "-")

  start_times <- times[[2L]][[2L]]
  start_times <- split_col(start_times, ":")

  end_times <- ifelse(
    is.na(times[[1L]][[1L]]), times[[2L]][[1L]], times[[1L]][[1L]]
  )
  end_times <- split_col(end_times, ":")

  list(
    date_start = dates[[2L]],
    date_end = dates[[1L]],
    hour_start = as.integer(start_times[[2L]]),
    hour_end = as.integer(end_times[[2L]]),
    minute_start = as.integer(start_times[[1L]]),
    minute_end = as.integer(end_times[[1L]])
  )

}

#' @noRd
split_coord_col <- function(col) {

  split_cols <- split_col(col, ":")

  lapply(split_cols, as.numeric)

}

#' @noRd
split_coord_euref_col <- function(col) {

  split_cols <- split_col(col, "\\D", 4L)

  split_cols <- lapply(split_cols, as.numeric)

  rev(split_cols)

}

#' @noRd
split_col <- function(col, split, n = 2L) {

  split_cols <- strsplit(as.character(col), split)
  split_cols <- lapply(split_cols, c, NA_character_)
  split_cols <- lapply(split_cols, utils::head, n)
  split_cols <- lapply(split_cols, rev)
  split_cols <- do.call(rbind, split_cols)

  lapply(seq_len(n), function(x) split_cols[, x])

}
