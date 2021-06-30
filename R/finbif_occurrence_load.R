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
#' @importFrom methods as
#' @importFrom tools file_ext
#' @export

finbif_occurrence_load <- function(
  file, select, n = -1, count_only = FALSE, quiet = FALSE,
  cache = getOption("finbif_use_cache"), dwc = FALSE, date_time_method,
  tzone = getOption("finbif_tz"), write_file = tempfile(), dt, keep_tsv = FALSE,
  facts = list(), type_convert_facts = TRUE, drop_na = FALSE
) {

  file <- preprocess_data_file(file)

  var_type <- col_type_string(dwc)

  select_all <- FALSE
  short <- FALSE
  deselect <- character()

  if (!missing(select) && select %in% c("all", "short")) {

    short <- identical(select[[1L]], "short")

    deselect <- grep("^-", select, value = TRUE)
    deselect <- gsub("-", "", deselect)
    deselect <- match(deselect, var_names[, var_type])
    deselect <- row.names(var_names[deselect, ])

    select <- "default_vars"
    select_all <- TRUE

  }

  defer_errors(select <- infer_selection("none", select, var_type))

  select[["deselect"]] <- deselect
  select[["all"]] <- select_all
  select[["type"]] <- var_type
  select[["facts"]] <- names(facts)

  n <- as.integer(n)

  df <- read_finbif_tsv(
    file, select, n, count_only, quiet, cache, write_file, dt, keep_tsv
  )

  if (count_only) {

    return(df)

  }

  n_recs <- attr(df, "nrow")

  url <- attr(df, "url")

  names(df) <- fix_issue_vars(names(df))

  file_vars <- attr(df, "file_vars")

  df <- new_vars(df, deselect, file_vars)

  record_id <- file_vars[["translated_var"]] == "record_id"

  record_id <- rownames(file_vars[record_id, ])

  record_id <- df[[record_id]]

  df <- expand_lite_cols(df)

  names(df) <- file_vars[names(df), var_type]

  for (i in names(df)) {

    df[[i]] <- add_nas(df, i, var_type, file_vars)

  }

  if (select_all) {

    select[["user"]] <- TRUE

  } else {

    date_time_method <- det_datetime_method(date_time_method, n_recs)

    df <- compute_date_time(
      df, select[["user"]], select[["user"]], aggregate = "none", dwc,
      date_time_method, tzone
    )

    df <- any_issues(df, select[["user"]], var_type)

    df <- compute_vars_from_id(df, select[["user"]])

    for (extra_var in setdiff(select[["user"]], names(df))) {

      ind <- var_names[[var_type]] == extra_var

      df[[extra_var]] <- methods::as(
        rep_len(NA, nrow(df)), var_names[ind, "type"]
      )

    }

  }

  attr(df, "file_cols") <- NULL
  attr(df, "file_vars") <- NULL

  df <- structure(
    df,
    class     = c("finbif_occ", class(df)),
    nrec_dnld = n_recs,
    nrec_avl  = n_recs,
    url       = url,
    time      = "??",
    short_nms = short,
    dwc       = dwc && !short,
    record_id = record_id
  )

  for (fact_type in names(facts)) {

    facts_df <- read_finbif_tsv(
      file,
      select = list(
        all = TRUE,
        deselect = character(),
        type = "translated_var"
      ),
      n = -1L,
      count_only, quiet, cache, write_file, dt, keep_tsv,
      facts = fact_type
    )

    id <- switch(
      fact_type,
      record = file_vars[["Unit.UnitID", var_type]],
      event = file_vars[["Gathering.GatheringID", var_type]],
      document = file_vars[["Document.DocumentID", var_type]]
    )

    facts_df <- spread_facts(
      facts_df, facts[[fact_type]], fact_type, id, type_convert_facts
    )

    select[["user"]] <- c(
      names(df[select[["user"]]]), setdiff(names(facts_df), id)
    )

    df <- bind_facts(df, facts_df)

  }

  if (short) {

    short_nms <- file_vars[["shrtnm"]]

    names(short_nms) <- file_vars[[var_type]]

    short_nms <- short_nms[names(df)]

    short_fcts <- unlist(facts)

    short_fcts <- gsub("http://tun.fi/", "", short_fcts)

    short_fcts <- abbreviate(
      short_fcts, 8L, FALSE, strict = TRUE, method = "both.sides"
    )
    short_fcts <- paste0("f", seq_along(short_fcts), short_fcts)

    short_nms[is.na(short_nms)] <- short_fcts

    names(df) <- short_nms

    select[["user"]] <- names(df)

  }

  df <- df[, select[["user"]], drop = FALSE]

  drop_na_col(df, drop_na)

}

#' @noRd
read_finbif_tsv <- function(
  file,
  select = list(all = TRUE, deselect = character(), type = "translated_var"),
  n = -1L, count_only = FALSE, quiet = FALSE, cache = TRUE,
  write_file = tempfile(), dt, keep_tsv, facts = "none"
) {

  file <- as.character(file)

  ptrn <- "http://tun.fi/HBF."

  is_url <- grepl(ptrn, file, fixed = TRUE)

  if (is_url) {

    file <- gsub(ptrn, "", file)

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
  tsv <- paste0(tsv_prefix, tsv)

  if (grepl("^[0-9]*$", file)) {

    url <- sprintf("https://dw.laji.fi/download/HBF.%s", file)

    tsv <- sprintf("%sHBF.%s.tsv", tsv_prefix, file)

    file <- get_zip(url, quiet, cache, write_file)

  } else {

    url <- file

  }

  df <- attempt_read(file, tsv, select, count_only, n, quiet, dt, keep_tsv)

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
attempt_read <- function(
  file, tsv, select, count_only, n, quiet, dt, keep_tsv
) {

  if (missing(dt)) {

    use_dt <- TRUE
    dt <- FALSE

  } else {

    use_dt <- dt

  }

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

        n_rows <- NULL

        if (!all) {

          n_rows <- nlines(i)

        }

        if (has_pkgs("data.table") && use_dt) {

          input <- as.character(i)

          df <- switch(
            tools::file_ext(input),
            tsv = dt_read(select, n, quiet, dt, input = input),
            dt_read(
              select, n, quiet, dt, keep_tsv,
              zip = list(input = input, tsv = tsv)
            )
          )

        } else {

          df <- rd_read(i, file, tsv, n, select, keep_tsv)

        }

        attr(df, "nrow") <- n_rows

        df

      },

      silent = TRUE

    )

    try(close(i), silent = TRUE)

    success <- !inherits(df, "try-error")

    if (success) {

      break

    }

  }

  stopifnot("invalid file or missing file(s)!" = success)

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
new_vars <- function(df, deselect, file_vars) {

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

  for (i in new_vars) {

    df[[i]] <- rep_len(NA, nrow(df))

  }

  df

}

#' @noRd
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

      on.exit({

        if (!is.null(zip)) {

          set_cache(zip, hash)

        }

      })

    } else {

      zip <- file.path(fcp, paste0("finbif_cache_file_", hash))

      if (file.exists(zip)) {

        return(zip)

      }

    }

  }

  progress <- NULL

  if (!quiet) {

    progress <- httr::progress()

  }

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

#' @noRd
add_nas <- function(df, nm, var_type, file_vars) {

  ans <- df[[nm]]

  if (all(is.na(ans))) {

    ind <- file_vars[[var_type]] == nm

    if (length(which(ind)) > 1L) {

      ind <- ind & file_vars[["superseeded"]] == "FALSE"

    }

    ans <- methods::as(ans, file_vars[ind, "type"])

  }

  ans

}

#' @noRd
any_issues <- function(df, select_user, var_type) {

  vnms <- var_names[var_type]
  any_issue <- vnms["unit.quality.documentGatheringUnitQualityIssues", ]

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
dt_read <- function(select, n, quiet, dt, keep_tsv = FALSE, ...) {

  args <- list(
    ..., nrows = 0, showProgress = quiet, data.table = dt, na.strings = "",
    quote = "\"", sep = "\t", fill = TRUE, check.names = FALSE, header = TRUE
  )

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

  if (select[["all"]]) {

    args[["select"]] <- which(!cols %in% deselect(select, file_vars))

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

    ind <- file_vars[["translated_vars"]] == "formatted_date_time"

    select[["query"]] <- switch(
      attr(file_vars, "locale"),
      none = select[["query"]],
      c(select[["queary"]], rownames(file_vars[ind, ]))
    )

    select_vars <-  var_names[select[["query"]], select[["type"]]]

    select_vars <- file_vars[[select[["type"]]]] %in% select_vars
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

  args[["colClasses"]] <- file_vars[cols, "type"]
  args[["colClasses"]] <- ifelse(
    is.na(args[["colClasses"]]), "character", args[["colClasses"]]
  )

  args[["nrows"]] <- as.double(n)
  args[["check.names"]] <- TRUE

  df <- do.call(data.table::fread, args)

  attr(df, "file_vars") <- file_vars

  attr(df, "file_cols") <- cols

  df

}

#' @noRd
rd_read <- function(x, file, tsv, n, select, keep_tsv) {

  df <- utils::read.delim(x, nrows = 1L, na.strings = "", quote = "\"")

  cols <- fix_issue_vars(names(df))

  file_vars <- infer_file_vars(cols)

  if (keep_tsv && !identical(tools::file_ext(file), "tsv")) {

    unzip <- "internal"

    if (!is.null(getOption("unzip")) && !identical(getOption("unzip"), "")) {

      unzip <- getOption("unzip")

    }

    utils::unzip(file, tsv, exdir = dirname(file), unzip = unzip)

  }

  if (identical(as.integer(n), 0L)) {

    df <- df[0L, ]

  } else {

    if (inherits(x, "connection")) {

      x <- unz(file, tsv)

    }

    df <- utils::read.delim(
      x, nrows = max(abs(n), 1L) * sign(n), na.strings = "",
      colClasses =  file_vars[cols, "type"], quote = "\""
    )

  }

  df <- df[!cols %in% deselect(select, file_vars)]

  attr(df, "file_vars") <- file_vars

  df

}

#' @noRd
deselect <- function(select, file_vars) {

  deselect <- var_names[select[["deselect"]], select[["type"]]]
  deselect <- file_vars[[select[["type"]]]] %in% deselect
  row.names(file_vars[deselect, ])

}

#' @noRd
spread_facts <-  function(facts, select, type, id, type_convert_facts) {

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

  facts

}

#' @noRd
bind_facts <- function(x, facts) {

  stopifnot(
    "Package {dplyr} is required for this functionality" = has_pkgs("dplyr")
  )

  id <- attr(facts, "id")

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

    col <- vapply(col, paste, character(1L), collapse = ", ")

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
infer_file_vars <- function(cols) {

  file_vars <- cite_file_vars

  locale <- "none"

  if (length(cols) < 100L && !"Fact" %in% cols) {

    file_vars <- lite_download_file_vars

    locale <- lapply(lite_download_file_vars, intersect, cols)
    locale <- lapply(locale, sort)
    locale <- vapply(locale, identical, logical(1L), sort(cols))

    stopifnot(
      "File has field names incompatible with this {finbif} R package version" =
        any(locale)
    )

    locale <- names(which(locale))[[1L]]

    rownames(file_vars) <- file_vars[[locale]]



  }

  attr(file_vars, "locale") <- locale

  file_vars

}

#' @noRd
preprocess_data_file <- function(file) {

  ext <- tools::file_ext(file)

  file <- switch(ext, ods = from_ods(file), xlsx = from_xlsx(file), file)

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

  write.table(df, file, quote = TRUE, sep = "\t", na = "", row.names = FALSE)

  file

}

#' @noRd
expand_lite_cols <- function(df) {

  file_vars <- attr(df, "file_vars")

  cols <- c(
    "formatted_taxon_name", "formatted_date_time", "coordinates_1_kkj",
    "coordinates_10_kkj", "coordinates_1_center_kkj",
    "coordinates_10_center_kkj"
  )

  cols <- which(file_vars[["translated_var"]] %in% cols)

  for (col in cols) {

    col_nm <- rownames(file_vars[col, ])

    type <- switch(
      file_vars[[col, "translated_var"]],
      formatted_taxon_name = "taxon",
      formatted_date_time = "date_time",
      "coords"
    )

    if (utils::hasName(df, col_nm)) {

      split_cols <- switch(
        type,
        taxon = split_taxa_col(df[[col_nm]], attr(file_vars, "locale")),
        date_time = split_dt_col(df[[col_nm]]),
        coords = split_coord_col(df[[col_nm]]),
      )

      new_cols <- switch(
        file_vars[[col, "translated_var"]],
        formatted_taxon_name = c(
          "scientific_name", "common_name_english", "common_name_finnish",
          "common_name_swedish"
        ),
        formatted_date_time = c(
          "date_start", "date_end", "hour_start", "hour_end",
          "minute_start", "minute_end"
        ),
        coordinates_1_kkj = c("lon_1_kkj", "lat_1_kkj"),
        coordinates_10_kkj = c("lon_10_kkj", "lat_10_kkj"),
        coordinates_1_center_kkj = c("lon_1_center_kkj", "lat_1_center_kkj"),
        coordinates_10_center_kkj = c("lon_10_center_kkj", "lat_10_center_kkj")
      )

      new_cols <- file_vars[["translated_var"]] %in% new_cols
      new_cols <- rownames(file_vars[new_cols, ])

      for (i in seq_along(new_cols)) {

        cond <- is.null(df[[new_cols[[i]]]]) || all(is.na(df[[new_cols[[i]]]]))

        if (cond) {

          df[[new_cols[[i]]]] <- split_cols[[i]]

        }

      }

    }

  }

  df

}

#' @noRd
split_taxa_col <- function(col, locale) {

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
  split_cols <- lapply(split_cols, as.numeric)

  names(split_cols) <- c("lon", "lat")

  split_cols

}

#' @noRd
split_col <- function(col, split) {

  split_cols <- strsplit(as.character(col), split)
  split_cols <- lapply(split_cols, c, NA_character_)
  split_cols <- lapply(split_cols, utils::head, 2L)
  split_cols <- lapply(split_cols, rev)
  split_cols <- do.call(rbind, split_cols)

  list(split_cols[, 1], split_cols[, 2])

}
