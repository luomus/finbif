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
#'   codes. Current supported languages are English, Finnish and Swedish. For
#'   data where more than one language is available the language denoted by
#'   `locale` will be preferred while falling back to the other languages in the
#'   order indicated above.
#' @param skip Integer. The number of lines of the data file to skip before
#'   beginning to read data (not including the header).
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

  nchars <- nchar(file)

  file <- switch(
    substring(file, nchars - 3L, nchars),
    .ods = from_ods(file),
    xlsx = from_xlsx(file),
    file
  )

  fb_records_obj <- list(
    file = file,
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

  all_cols <- any(c("all", "short") %in% select)

  var_names <- sysdata("var_names")

  var_type <- col_type_string(dwc)

  col_names <- var_names[[var_type]]

  short <- FALSE

  deselect <- character()

  if (all_cols) {

    short <- identical(select[[1L]], "short")

    deselect <- grep("^-", select, value = TRUE)

    deselect <- gsub("-", "", deselect)

    deselect <- match(deselect, col_names)

    deselect <- row.names(var_names[deselect, ])

    select <- "default_vars"

  }

  fb_records_obj[["aggregate"]] <- "none"

  fb_records_obj[["select"]] <- select

  fb_records_obj[["include_facts"]] <- FALSE

  fb_records_obj[["var_type"]] <- var_type

  defer_errors({

    select <- infer_selection(fb_records_obj)

  })

  fact_types <- vapply(facts, length, 0L) > 0L

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

  select <- select_facts(select)

  attr(fb_occurrence_df, "select") <- select

  fb_occurrence_df <- new_vars(fb_occurrence_df)

  record_id <- file_vars[["translated_var"]] == "record_id"

  record_id <- file_vars[record_id, ]

  record_id <- rownames(record_id)

  record_id <- fb_occurrence_df[[record_id]]

  fb_occurrence_df <- expand_lite_cols(fb_occurrence_df)

  df_names <- names(fb_occurrence_df)

  nms <- file_vars[df_names, var_type]

  na_nms <- is.na(nms)

  names(fb_occurrence_df) <- ifelse(na_nms, df_names, nms)

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

  select[["user"]] <- names(fb_occurrence_df)

  n_recs <- attr(fb_occurrence_df, "nrow", TRUE)

  if (!all_cols) {

    select[["user"]] <- select_user

    datetime_obj <- list(date_time_method = date_time_method, n = n_recs)

    datetime_obj <- det_datetime_method(datetime_obj)

    fb_occurrence_df <- structure(
      fb_occurrence_df,
      select_user = select_user,
      column_names = select_user,
      aggregate = "none",
      dwc = dwc,
      date_time = select[["date_time_selected"]],
      date_time_method = datetime_obj[["date_time_method"]],
      tzone = tzone
    )

    fb_occurrence_df <- date_times(fb_occurrence_df)

    fb_occurrence_df <- compute_date_time(fb_occurrence_df)

    fb_occurrence_df <- compute_duration(fb_occurrence_df)

    fb_occurrence_df <- compute_iso8601(fb_occurrence_df)

    fb_occurrence_df <- any_issues(fb_occurrence_df)

    df_names <- names(fb_occurrence_df)

    nrows <- nrow(fb_occurrence_df)

    na <- rep_len(NA, nrows)

    for (extra_var in setdiff(select_user, df_names)) {

      type <- var_names[col_names == extra_var, "type"]

      fb_occurrence_df[[extra_var]] <- cast_to_type(na, type)

    }

  }

  attr(fb_occurrence_df, "file_cols") <- NULL

  attr(fb_occurrence_df, "file_vars") <- NULL

  class <- class(fb_occurrence_df)

  fb_occurrence_df <- structure(
    fb_occurrence_df,
    class = c("finbif_occ", class),
    nrec_dnld = n_recs,
    nrec_avl = n_recs,
    url = attr(fb_occurrence_df, "url", TRUE),
    time = "??",
    short_nms = short,
    dwc = dwc && !short,
    record_id = record_id
  )

  for (ftype in select[["facts"]]) {

    stopifnot("Invalid fact type" = ftype %in% c("record", "event", "document"))

    fb_records_obj[["select"]] <- list(
      all = TRUE,
      deselect = character(),
      type = "translated_var"
    )

    fb_records_obj[["n"]] <- -1L

    fb_records_obj[["facts"]] <- ftype

    id <- switch(
      ftype,
      record = file_vars[["Unit.UnitID", var_type]],
      event = file_vars[["Gathering.GatheringID", var_type]],
      document = file_vars[["Document.DocumentID", var_type]]
    )

    facts_df <- structure(
      try(read_finbif_tsv(fb_records_obj), silent = TRUE),
      facts = facts[[ftype]],
      fact_type = ftype,
      id = id,
      type_convert_facts = type_convert_facts,
      drop_facts_na = drop_facts_na
    )

    facts_df <- spread_facts(facts_df)

    select_user <- select[["user"]]

    df_names_user <- names(fb_occurrence_df[select_user])

    df_names_facts <- names(facts_df)

    df_names_facts <- setdiff(df_names_facts, id)

    select[["user"]] <- c(df_names_user, df_names_facts)

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

    n <- length(short_fcts) + .1

    n <- log10(n)

    short_fcts <- abbreviate(
      short_fcts, 9 - ceiling(n), FALSE, strict = TRUE, method = "both.sides"
    )

    short_fact_sq <- seq_along(short_fcts)

    short_nms[is.na(short_nms)] <- paste0("f", short_fact_sq, short_fcts)

    missing <- which(short_nms == "f")

    nms_missing <- df_names[missing]

    nms_missing <- gsub("[^A-Za-z]", "", nms_missing)

    short_nms[missing] <- abbreviate(nms_missing, 10L)

    select[["user"]] <- short_nms

    names(fb_occurrence_df) <- short_nms

  }


  select_user <- name_chr_vec(select[["user"]])

  fb_occurrence_df <- fb_occurrence_df[, select_user, drop = FALSE]

  attr(fb_occurrence_df, "column_names") <- select_user

  attr(fb_occurrence_df, "drop_na") <- drop_na

  drop_na_col(fb_occurrence_df)

}

#' @noRd

read_finbif_tsv <- function(fb_occurrenc_obj) {

  file <- as.character(fb_occurrenc_obj[["file"]])

  ptrn <- "^https?://.+?/HBF\\."

  if (grepl(ptrn, file)) {

    file <- sub(ptrn, "", file)

  }

  tsv <- basename(file)

  tsv <- gsub("zip", "tsv", tsv)

  facts <- fb_occurrenc_obj[["facts"]]

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

  fb_occurrenc_obj[["tsv"]] <-  paste0(tsv_prefix, tsv)

  if (grepl("^[0-9]*$", file)) {

    fb_occurrenc_obj[["tsv"]] <- sprintf("%sHBF.%s.tsv", tsv_prefix, file)

    finbif_dl_url <- getOption("finbif_dl_url")

    fb_occurrenc_obj[["url"]] <- sprintf("%s/HBF.%s", finbif_dl_url, file)

    fb_occurrenc_obj <- get_zip(fb_occurrenc_obj)

    file <- fb_occurrenc_obj[["file"]]

  }

  df <- attempt_read(fb_occurrenc_obj)

  if (fb_occurrenc_obj[["count_only"]]) {

    return(df)

  }

  attr(df, "url") <- file

  if (identical(fb_occurrenc_obj[["n"]], -1L)) {

    attr(df, "nrow") <- nrow(df)

  }

  df

}

#' @noRd

attempt_read <- function(fb_occurrence_obj) {

  use_dt <- fb_occurrence_obj[["dt"]]

  if (is.na(use_dt)) {

    use_dt <- TRUE

    fb_occurrence_obj[["dt"]] <- FALSE

  }

  if (fb_occurrence_obj[["count_only"]]) {

    ans <- nlines(fb_occurrence_obj)

  } else {

    n_rows <- NULL

    if (!identical(fb_occurrence_obj[["n"]], -1L)) {

      n_rows <- nlines(fb_occurrence_obj)

    }

    if (use_dt && has_pkgs("data.table")) {

      input <- as.character(fb_occurrence_obj[["file"]])

      input_list <- list(input = input, tsv = fb_occurrence_obj[["tsv"]])

      input_list <- list(zip = input_list)

      if (grepl("\\.tsv$", input)) {

        fb_occurrence_obj[["keep_tsv"]] <- FALSE

        input_list <- list(input = input)

      }

      fb_occurrence_obj[["dt_args"]] <- input_list

      ans <- dt_read(fb_occurrence_obj)

    } else {

      ans <- rd_read(fb_occurrence_obj)

    }

    attr(ans, "nrow") <- n_rows

  }

  ans

}

#' @noRd

localise_enums <- function(df) {

  file_vars <- attr(df, "file_vars", TRUE)

  field_var_names <- row.names(file_vars)

  labels_obj <- list(var_names = file_vars, locale = attr(df, "locale", TRUE))

  for (nm in names(df)) {

    if (nm %in% field_var_names && isTRUE(file_vars[[nm, "localised"]])) {

      labels_obj[["labels"]] <- df[[nm]]

      labels_obj[["col"]] <- nm

      df[[nm]] <- localise_labels(labels_obj)

    }

  }

  df

}

#' @noRd

fix_issue_vars <- function(x) {

  type <- c("Time", "Location")

  for (i in c("Issue", "Source", "Message")) {

    for (j in 1:2) {

      issue <- sprintf("Issue.%s.%s", i, j)

      issue_type <- sprintf("%sIssue.%s", type[[j]], i)

      x <- sub(issue, issue_type, x)

    }

  }

  x

}

#' @noRd

new_vars <- function(df) {

  file_vars <- attr(df, "file_vars", TRUE)

  nss <- file_vars[["superseeded"]] == "FALSE"

  ss <- rownames(file_vars[!nss, ])

  file_cols <- attr(df, "file_cols", TRUE)

  if (is.null(file_cols)) {

    file_cols <- names(df)

    attr(df, "file_cols") <- file_cols

  }

  ss <- intersect(ss, file_cols)

  var_names <- sysdata("var_names")

  select <- attr(df, "select", TRUE)

  ds <- select[["deselect"]]

  ds <- file_vars[["translated_var"]] %in% var_names[ds, "translated_var"]

  nms <- row.names(file_vars[nss & !ds, ])

  file_cols <- c(file_cols, file_vars[ss, "superseeded"])

  if (!select[["all"]] && nrow(df) > 0L) {

    df[setdiff(nms, file_cols)] <- NA

  }

  df

}

#' @noRd
#' @importFrom digest digest
#' @importFrom httr RETRY progress write_disk

get_zip <- function(fb_occurrenc_obj) {

  write_file <- fb_occurrenc_obj[["write_file"]]

  url <- fb_occurrenc_obj[["url"]]

  if (fb_occurrenc_obj[["cache"]] > 0) {

    hash <- sub(":\\d+", "", url)

    hash <- digest::digest(hash)

    fcp <- getOption("finbif_cache_path")

    if (is.null(fcp)) {

      cache_file <- get_cache(hash)

      if (!is.null(cache_file)) {

        fb_occurrenc_obj[["file"]] <- cache_file

        return(fb_occurrenc_obj)

      }

      on.exit({

        if (!is.null(write_file)) {

          cache_obj <- list(data = write_file, hash = hash, timeout = Inf)

          set_cache(cache_obj)

        }

      })

    } else if (is.character(fcp)) {

      file_name <- paste0("finbif_dwnld_cache_file_", hash)

      write_file <- file.path(fcp, file_name)

      if (file.exists(write_file)) {

        fb_occurrenc_obj[["file"]] <- write_file

        return(fb_occurrenc_obj)

      }

    } else {

      stop("Database cache cannot be used for FinBIF downloads.", call. = TRUE)

    }

  }

  progress <- NULL

  if (!fb_occurrenc_obj[["quiet"]]) {

    progress <- httr::progress()

  }

  allow <- getOption("finbif_allow_query")

  stopifnot("Request not cached and option:finbif_allow_query = FALSE" = allow)

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
    times =  getOption("finbif_retry_times"),
    pause_base = getOption("finbif_retry_pause_base"),
    pause_cap = getOption("finbif_retry_pause_cap"),
    pause_min = getOption("finbif_retry_pause_min"),
    quiet = fb_occurrenc_obj[["quiet"]],
    terminate_on = 404L
  )

  fs <- file.size(write_file)

  fl <- Sys.getenv("FINBIF_FILE_SIZE_LIMIT")

  fl <- as.integer(fl)

  if (isTRUE(fs > fl)) {

    stop("File download too large; err_name: too_large", call. = FALSE)

  }

  code <- resp[["status_code"]]

  if (!identical(code, 200L)) {

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

  file_vars <- attr(df, "file_vars", TRUE)

  file_var_type <- file_vars[[var_type]]

  vnames <- sysdata("var_names")

  vnames_type <- vnames[[var_type]]

  for (nm in names(df)) {

    if (all_na(df[[nm]])) {

      ind <- file_var_type == nm & file_vars[["superseeded"]] == "FALSE"

      if (any(ind)) {

        df[[nm]] <- cast_to_type(df[[nm]], file_vars[ind, "type"])

      } else if (nm %in% vnames_type) {

        df[[nm]] <- cast_to_type(df[[nm]], vnames[vnames_type == nm, "type"])

      }

    }

  }

  df

}

#' @noRd

any_issues <- function(df) {

  dwc <- attr(df, "dwc", TRUE)

  vtype <- col_type_string(dwc)

  vnms <- sysdata("var_names")

  issues <- vnms[["unit.quality.documentGatheringUnitQualityIssues", vtype]]

  if (issues %in% attr(df, "select_user", TRUE) && !issues %in% names(df)) {

    issue <- logical()

    if (nrow(df) > 0L) {

      issue <- FALSE

      issue_cols <- c(
        "unit.quality.issue.issue",
        "gathering.quality.issue.issue",
        "gathering.quality.timeIssue.issue",
        "gathering.quality.locationIssue.issue"
      )

      has_an_issue_col <- FALSE

      for (i in issue_cols) {

        issue_col_nm <- vnms[[i, vtype]]

        issue_col <- df[[issue_col_nm]]

        has_issue_col <- length(issue_col) > 0L

        if (has_issue_col) {

          issue <- issue | !is.na(issue_col)

        }

        has_an_issue_col <- has_an_issue_col || has_issue_col

      }

      if (!has_an_issue_col) {

        issue <- NA

      }

    }

    df[[issues]] <- issue

  }

  df

}

#' @noRd
#' @importFrom utils unzip

dt_read <- function(fb_occurrence_obj) {

  skip <- fb_occurrence_obj[["skip"]]

  args <- list(
    nrows = 0,
    showProgress = !fb_occurrence_obj[["quiet"]],
    data.table = fb_occurrence_obj[["dt"]],
    na.strings = "",
    quote = "",
    sep = "\t",
    fill = TRUE,
    check.names = FALSE,
    header = TRUE,
    skip = 0L
  )

  args <- c(fb_occurrence_obj[["dt_args"]], args)

  if ("zip" %in% names(args)) {

    unzip <- op_unzip()

    zip_input <- args[[c("zip", "input")]]

    zip_tsv <- args[[c("zip", "tsv")]]

    dir <- dirname(zip_input)

    args_input <- sprintf("%s/%s", dir, zip_tsv)

    args[["input"]] <- args_input

    if (!file.exists(args_input)) {

      utils::unzip(zip_input, files = zip_tsv, exdir = dir, unzip = unzip)

      if (!fb_occurrence_obj[["keep_tsv"]]) {

        on.exit(unlink(args_input))

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

  select <- fb_occurrence_obj[["select"]]

  select[["file_vars"]] <- file_vars

  if (attr(file_vars, "lite", TRUE)) {

    args[["quote"]] <- "\""

  }

  if (select[["all"]]) {

    args_select <- !cols %in% deselect(select)

    args[["select"]] <- which(args_select)

  } else {

    select_query <- select[["query"]]

    if ("unit.quality.documentGatheringUnitQualityIssues" %in% select_query) {

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

    vnms <- sysdata("var_names")

    select_vars <- file_vars[[select_type]] %in% vnms[select_query, select_type]

    expand_vars <- c(
      "formatted_taxon_name",
      "formatted_date_time",
      "coordinates_euref",
      "coordinates_1_ykj",
      "coordinates_10_ykj",
      "coordinates_1_center_ykj",
      "coordinates_10_center_ykj"
    )

    select_vars <- switch(
      attr(file_vars, "locale", TRUE),
      none = select_vars,
      file_vars[["translated_var"]] %in% expand_vars | select_vars
    )

    args_select <- cols %in% row.names(file_vars[select_vars, ])

    args_select <- which(args_select)

    for (ftype in select[["facts"]]) {

      id_col <- switch(
        ftype,
        record = "Unit.UnitID",
        event = "Gathering.GatheringID",
        document = "Document.DocumentID"
      )

      id_col <- which(cols %in% id_col)

      args_select  <- c(args_select, id_col)

    }

    args_select <- unique(args_select)

    args_select <- sort(args_select)

    args[["select"]] <- args_select

  }

  args[["nrows"]] <- as.double(fb_occurrence_obj[["n"]])

  args[["check.names"]] <- TRUE

  args[["skip"]] <- skip + 1L

  args[["header"]] <- FALSE

  df <- do.call(data.table::fread, args)

  names(df) <- cols[args_select]

  classes <- file_vars[cols, "type"]

  classes <- classes[args_select]

  na_classes <- is.na(classes)

  classes <- ifelse(na_classes, "character", classes)

  for (i in seq_along(df)) {

    df[[i]] <- cast_to_type(df[[i]], classes[[i]])

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

  if (fb_occurrence_obj[["keep_tsv"]] && !grepl("\\.tsv$", file)) {

    unzip <- op_unzip()

    dir <- dirname(file)

    utils::unzip(file, tsv, exdir = dir, unzip = unzip)

  }

  connection_obj <- list(file = file, tsv = tsv, mode = "")

  quote <- ""

  df <- utils::read.delim(
    open_tsv_connection(connection_obj),
    nrows = 1L,
    na.strings = "",
    quote = quote,
    skip = 0L
  )

  df_names <- names(df)

  cols <- fix_issue_vars(df_names)

  file_vars <- infer_file_vars(cols)

  select <- fb_occurrence_obj[["select"]]

  select[["file_vars"]] <- file_vars

  if (attr(file_vars, "lite", TRUE)) {

    quote <- "\""

  }

  n <- as.integer(fb_occurrence_obj[["n"]])

  if (identical(n, 0L)) {

    df <- df[0L, ]

  } else {

    connection_obj <- list(file = file, tsv = tsv, mode = "")

    df <- utils::read.delim(
      open_tsv_connection(connection_obj),
      header = FALSE,
      quote = quote,
      na.strings = "",
      nrows = n,
      skip = fb_occurrence_obj[["skip"]] + 1L
    )

    classes <- file_vars[cols, "type"]

    na_classes <- is.na(classes)

    classes <- ifelse(na_classes, "character", classes)

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

op_unzip <- function() {

  unzip <- "internal"

  op_unzip <- getOption("unzip")

  if (!is.null(op_unzip) && !identical(op_unzip, "")) {

    unzip <- op_unzip

  }

  unzip

}

#' @noRd

deselect <- function(select) {

  file_vars <- select[["file_vars"]]

  type <- select[["type"]]

  deselect <- select[["deselect"]]

  var_names <- sysdata("var_names")

  ind <- file_vars[[type]] %in% var_names[deselect, type]

  row.names(file_vars[ind, ])

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

  select_facts <- facts[["Fact"]]

  ind <- match(select, select_facts)

  fact_names <- names(facts)

  id_col <- fact_names == "Parent"

  fact_names[id_col] <- id

  names(facts) <- fact_names

  na_ind <- is.na(ind)

  if (any(na_ind)) {

    missing_facts <- select[na_ind]

    warning <- paste(missing_facts, collapse = ", ")

    warning(
      "Selected fact(s) - ",
      warning,
      " - could not be found in dataset",
      call. = FALSE
    )

    missing_facts <- missing_facts[!isTRUE(drop_facts_na)]

  }

  if (!all(na_ind)) {

    select <- select[!na_ind]

    facts <- facts[select_facts %in% select, ]

    facts[["Fact"]] <- paste(type, "fact_", facts[["Fact"]], sep = "_")

    facts <- tapply(facts[["Value"]], facts[c("Fact", id)], c, simplify = FALSE)

    fact_dimnames <- dimnames(facts)

    selected_fact_nms <- paste(type, "fact_", select, sep = "_")

    fact_nms <- intersect(selected_fact_nms, fact_dimnames[["Fact"]])

    colnames <- c(id, fact_nms)

    ncols <- length(colnames)

    fact_list <- vector("list", ncols)

    names(fact_list) <- colnames

    ids <- fact_dimnames[[id]]

    fact_list[[id]] <- ids

    for (i in fact_nms) {

      fact_i <- facts[i, ]

      fact_i <- unname(fact_i)

      fact_i[vapply(fact_i, is.null, NA)] <- NA

      fact_i <- unlist_col(fact_i)

      if (type_convert_facts) {

        fact_i <- convert_col_type(fact_i)

      }

      fact_list[[i]] <- fact_i

    }

    facts <- structure(
      fact_list, class = "data.frame", row.names = seq_along(ids)
    )

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

  facts <- attr(x, "facts_df", TRUE)

  id <- attr(facts, "id", TRUE)

  stopifnot("Cannot bind facts. ID column missing from data" = id %in% names(x))

  matches <- match(x[[id]], facts[[id]])

  facts[[id]] <- NULL

  facts <- facts[matches, , drop = FALSE]

  ans <- cbind(x, facts)

  attr <- attributes(x)

  attr[["names"]] <- names(ans)

  attributes(ans) <- attr

  ans

}

#' @noRd

short_nms <- function(file_vars) {

  var_type <- attr(file_vars, "var_type", TRUE)

  short_nms <- c(file_vars[["shrtnm"]], "abund", "crdUncert", "sciNm")

  translated_var <- c("abundance", "coordinates_uncertainty", "scientific_name")

  dwc <- c("individualCount", "coordinateUncertaintyInMeters", "scientificName")

  nms <- switch(var_type, translated_var = translated_var, dwc = dwc)

  names(short_nms) <- c(file_vars[[var_type]], nms)

  short_nms

}

#' @noRd

unlist_col <- function(col) {

  col_unlisted <- unlist(col)

  col_len <- length(col)

  col_unlisted_len <- length(col_unlisted)

  if (identical(col_len, col_unlisted_len)) {

    col_unlisted

  } else {

    col

  }

}

#' @noRd

convert_col_type <- function(col) {

  if (is.list(col)) {

    col <- vapply(col, paste_col, "")

  }

  col[col == ""] <- NA_character_

  nws <- trimws(col)

  nws_no_na <- nws[!is.na(col)]

  num <- grepl("^[-+]?[0-9]*[\\.,]?[0-9]+([eE][-+]?[0-9]+)?$", nws_no_na)

  if (all(num)) {

    col <- as.numeric(nws)

    int <- !grepl("[\\.,]", nws_no_na)

    if (all(int)) {

      col <- as.integer(nws)

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

  if (length(cols) < 100L && !"Fact" %in% cols) {

    file_vars <- sysdata("lite_download_file_vars")

    locale <- lapply(file_vars, intersect, cols)

    locale <- vapply(locale, length, 0L)

    locale <- locale == max(locale)

    locale <- which(locale)

    locale_length <- length(locale)

    one_locale <- identical(locale_length, 1L)

    stopifnot(
      "Field names incompatible with this {finbif} package version" = one_locale
    )

    locale_nms <- names(locale)

    locale <- locale_nms[[1L]]

    rownames(file_vars) <- file_vars[[locale]]

    attr(file_vars, "lite") <- TRUE

    attr(file_vars, "locale") <- locale

  } else {

    file_vars <- sysdata("cite_file_vars")

    attr(file_vars, "lite") <- FALSE

    attr(file_vars, "locale") <- "none"

  }

  file_vars

}

#' @noRd

nlines <- function(fb_occurrence_obj) {

  connection_obj <- list(
    file = fb_occurrence_obj[["file"]],
    tsv = fb_occurrence_obj[["tsv"]],
    mode = "rb"
  )

  con <- open_tsv_connection(connection_obj)

  on.exit(close(con))

  n <- -1L

  cond <- TRUE

  while (cond) {

    chunk <- readBin(con, "raw", 65536L)

    chunk_10 <- chunk == as.raw(10L)

    n <- n + sum(chunk_10)

    empty <- raw(0L)

    cond <- !identical(chunk, empty)

  }

  n

}

#' @noRd

open_tsv_connection <- function(connection_obj) {

  file <- connection_obj[["file"]]

  mode <- connection_obj[["mode"]]

  nchars <- nchar(file)

  switch(
    substring(file, nchars - 3L, nchars),
    .tsv = file(file, mode),
    unz(file, connection_obj[["tsv"]], mode)
  )

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

  if (!select[["all"]]) {

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

    cols <- c(
      "formatted_taxon_name",
      "formatted_date_time",
      "coordinates_euref",
      "coordinates_1_ykj",
      "coordinates_10_ykj",
      "coordinates_1_center_ykj",
      "coordinates_10_center_ykj"
    )

    translated_vars <- file_vars[["translated_var"]]

    for (col in which(translated_vars %in% cols)) {

      col_nm <- rownames(file_vars[col, ])

      df_col <- df[[col_nm]]

      df_col_na <- is.na(df_col)

      has_col_nm <- col_nm %in% df_names && !all(df_col_na)

      if (has_col_nm) {

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

        new_cols <- rownames(file_vars[translated_vars %in% new_cols, ])

        for (i in seq_along(new_cols)) {

          col <- new_cols[[i]]

          dfi <- df[[col]]

          na_dfi <- is.na(dfi)

          no_col <- is.null(dfi) || all(na_dfi)

          if (no_col) {

             df[[col]] <- split_cols[[i]]

          }

        }

      }

    }

  }

  df

}

#' @noRd

split_taxa_col <- function(col) {

  col_list <- list(col, n = 2L, split = " \u2014 ")

  split_cols <- split_col(col_list)

  col_list2 <- list(split_cols[[2L]], n = 2L, split = " \\(|\\)")

  common_names <- split_col(col_list2)

  common_names1 <- common_names[[1L]]

  common_names2 <- common_names[[2L]]

  common_names_na <- is.na(common_names1)

  locale <- attr(col, "locale", TRUE)

  common_names1 <- ifelse(common_names_na, locale, common_names1)

  split_cols <- list(scientific_name = split_cols[[1L]])

  for (loc in c("en", "fi", "sv")) {

    ind <- common_names1 == loc

    col_loc <- NA_character_

    col_loc[ind] <- common_names2[ind]

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

  times <- lapply(col, "[[", 1L)

  times <- lapply(times, list, n = 2L, split = "-")

  times <- lapply(times, split_col)

  times1 <- times[[1L]]

  times1 <- times1[[1L]]

  times2 <- times[[2L]]

  start_times <- list(times2[[2L]], n = 2L, split = ":")

  start_times <- split_col(start_times)

  times_na <- is.na(times1)

  end_times <- ifelse(times_na, times2[[1L]], times1)

  end_times <- list(end_times, n = 2L, split = ":")

  end_times <- split_col(end_times)

  list(
    date_start = dates2,
    date_end = ifelse(dates_na, dates2, dates1),
    hour_start = as.integer(start_times[[2L]]),
    hour_end =  as.integer(end_times[[2L]]),
    minute_start = as.integer(start_times[[1L]]),
    minute_end = as.integer(end_times[[1L]])
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

  col <- as.character(split_obj[[1L]])

  sq <- seq_len(split_obj[["n"]])

  split_cols <- strsplit(col, split_obj[["split"]])

  split_cols <- lapply(split_cols, c, NA_character_)

  split_cols <- lapply(split_cols, "[", sq)

  split_cols <- lapply(split_cols, rev)

  split_cols <- do.call(rbind, split_cols)

  apply(split_cols, 2L, c, simplify = FALSE)

}
