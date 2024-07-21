#' @noRd
#' @importFrom digest digest
#' @importFrom httr content RETRY
#' @importFrom utils packageVersion

api_get <- function(obj) {

  fb_access_token <- get_token()

  fb_restricted_access_token <- get_restricted_access_token(obj)

  url <- getOption("finbif_api_url")

  version <- getOption("finbif_api_version")

  path <- obj[["path"]]

  query <- obj[["query"]]

  obj[["timeout"]] <- get_timeout(obj)

  hash <- NULL

  if (obj[["cache"]] > 0) {

    query_list <- list(url, version, path, query)

    hash <- digest::digest(query_list)

    fcp <- getOption("finbif_cache_path")

    if (is.null(fcp)) {

      cached_obj <- get_cache(hash)

      if (!is.null(cached_obj)) {

        cached_obj[["from_cache"]] <- TRUE

        return(cached_obj)

      }

      on.exit(cache_obj(obj))

    } else if (is.character(fcp)) {

      cache_file_name <- paste0("finbif_cache_file_", hash)

      cache_file_path <- file.path(fcp, cache_file_name)

      obj[["cache_file_path"]] <- cache_file_path

      if (file.exists(cache_file_path)) {

        created <- file.mtime(cache_file_path)

        cached_obj <- readRDS(cache_file_path)

        if (cache_is_valid(cached_obj[["timeout"]], created)) {

          cached_obj[["from_cache"]] <- TRUE

          return(cached_obj)

        } else {

          unlink(cache_file_path)

        }

      }

      on.exit(save_obj(obj))

    } else {

      stopifnot(
        "{DBI} & {blob} needed to use a DB cache" =  has_pkgs("DBI", "blob")
      )

      if (!DBI::dbExistsTable(fcp, "finbif_cache")) {

        init <- data.frame(
          hash = character(),
          created = as.POSIXct(numeric()),
          timeout = numeric(),
          blob = blob::blob()
        )

        DBI::dbWriteTable(fcp, "finbif_cache", init)

      } else {

        db_query <- sprintf(
          "SELECT * FROM finbif_cache WHERE hash = '%s'", hash
        )

        db_cache <- DBI::dbGetQuery(fcp, db_query)

        nrows <- nrow(db_cache)

        if (nrows > 0L) {

          created <- as.POSIXct(db_cache[["created"]], origin = "1970-01-01")

          ind <- which.max(created)

          ind <- ind[[1L]]

          if (cache_is_valid(db_cache[[ind, "timeout"]], created[[ind]])) {

            cached_obj <- db_cache[ind, "blob"]

            debug_msg(
              "INFO [",
              format(Sys.time()),
              "] ",
              "Reading from cache: ",
              hash
            )

            cached_obj <- unserialize(cached_obj[[1L]])

            cached_obj[["from_cache"]] <- TRUE

            return(cached_obj)

          } else {

            db_query <- sprintf(
              "DELETE FROM finbif_cache WHERE hash = '%s'", hash
            )

            debug_msg(
              "INFO [", format(Sys.time()), "] ", "Removing from cache: ", hash
            )

            DBI::dbExecute(fcp, db_query)

          }

        }

      }

      on.exit(append_obj(obj))

    }

  }

  allow <- getOption("finbif_allow_query")

  stopifnot("Request not cached and option:finbif_allow_query = FALSE" = allow)

  query <- add_email(query)

  fb_restricted_access_token_par <- list(
    permissionToken = fb_restricted_access_token
  )

  query <- switch(
    fb_restricted_access_token,
    unset = query,
    c(query, fb_restricted_access_token_par)
  )

  Sys.sleep(1 / getOption("finbif_rate_limit"))

  private_api <- Sys.getenv("FINBIF_PRIVATE_API", "unset")

  private_api <- switch(
    private_api,
    unset = "laji.fi",
    sprintf("%s.%s", private_api, "laji.fi")
  )

  use_private_api <- Sys.getenv("FINBIF_USE_PRIVATE_API")

  url_path <- switch(
    tolower(use_private_api),
    true = sprintf("https://%s/api/%s", private_api, path),
    sprintf("%s/%s/%s", url, version, path)
  )

  url_path <- switch(
    path,
    swagger = sprintf("%s/explorer/swagger.json", url),
    url_path
  )

  pkg_version <- utils::packageVersion("finbif")

  calling_fun <- get_calling_function("finbif")

  agent <- paste0("https://github.com/luomus/finbif#", pkg_version)

  agent <- paste0(agent, ":", calling_fun)

  agent <- list(useragent = Sys.getenv("FINBIF_USER_AGENT", agent))

  config <- list(headers = c(Accept = "application/json"), options = agent)

  fb_access_token_par <- list(access_token = fb_access_token)

  query <- switch(use_private_api, true = query, c(query, fb_access_token_par))

  resp <- httr::RETRY(
    "GET",
    url_path,
    structure(config, class = "request"),
    query = switch(path, swagger = list(), query),
    times = getOption("finbif_retry_times"),
    pause_base = getOption("finbif_retry_pause_base"),
    pause_cap = getOption("finbif_retry_pause_cap"),
    pause_min = getOption("finbif_retry_pause_min"),
    terminate_on = 404L
  )

  fb_access_token_str <- paste0("&access_token=", fb_access_token)

  notoken <- gsub(fb_access_token_str, "", resp[["url"]])

  email <- getOption("finbif_email")

  email_str <- paste0("&personEmail=", email)

  notoken <- gsub(email_str, "", notoken)

  fb_restricted_access_token_str <- paste0(
    "&permissionToken=", fb_restricted_access_token
  )

  notoken <- gsub(fb_restricted_access_token_str, "", notoken)

  resp[["url"]] <- notoken

  resp[[c("request", "url")]] <- notoken

  parsed <- httr::content(resp)

  if (!identical(resp[["status_code"]], 200L)) {

    obj <- NULL

    err_msg <- paste0(
      "API request failed [", resp[["status_code"]], "]\n", parsed[["message"]]
    )

    stop(err_msg, call. = FALSE)

  }

  obj[["content"]] <- parsed

  obj[["response"]] <- resp

  obj[["hash"]] <- hash

  obj[["from_cache"]] <- FALSE

  debug_msg(
    "INFO [", format(Sys.time()), "] ", "Request made to: ", notoken, " ", hash
  )

  structure(obj, class = "finbif_api")

}

#' @noRd

get_calling_function <- function(pkg) {

  for (call in sys.calls()) {

    fun <- try(as.character(call[[1L]]), silent = TRUE)

    if (inherits(fun, "character")) {

      len <- length(fun)

      fun <- fun[[len]]

      ns <- getNamespace(pkg)

      if (fun %in% ls(ns)) {

        break

      }

    }

  }

  args <- call[-1L]

  arg_nm_strs <- ""

  if (length(args) > 0L) {

    type <- vapply(args, typeof, "")

    len <- vapply(args, length, 0L)

    arg_nms <- names(args)

    arg_nm_strs <- paste0(arg_nms, "=", type, "<", len, ">")

  }

  arg_nm_str <- paste(arg_nm_strs, collapse = ",")

  paste0(fun, "(", arg_nm_str, ")")

}

#' @noRd

add_email <- function(query) {

  email <- getOption("finbif_email")

  use_private_api <- Sys.getenv("FINBIF_USE_PRIVATE_API")

  use_private_api <- as.logical(use_private_api)

  if (!is.null(email) && !isTRUE(use_private_api)) {

    email_par <- list(personEmail = email)

    query <- c(query, email_par)

  }

  query

}

#' @noRd

get_token <- function() {

  fb_access_token <- Sys.getenv("FINBIF_ACCESS_TOKEN")

  use_private_api <- Sys.getenv("FINBIF_USE_PRIVATE_API")

  use_private_api <- as.logical(use_private_api)

  if (identical(fb_access_token, "") && !isTRUE(use_private_api)) {

    stop(
      "Access token for FinBIF has not been set. Use finbif_get_token() to \n",
      "have an access token sent to your email address. Then set it as the \n",
      "environment variable FINBIF_ACCESS_TOKEN with \n",
      "Sys.setenv(FINBIF_ACCESS_TOKEN = \"<access_token_sent_to_your_email>\")",
      call. = FALSE
    )

  }

  fb_access_token

}

#' @noRd

get_timeout <- function(obj) {

  timeout <- obj[["cache"]]

  if (is.logical(timeout) || isTRUE(obj[["cache_override"]])) {

    timeout <- Inf

  }

  if (isFALSE(obj[["cache_override"]])) {

    timeout <- 0

  }

  timeout

}

#' @noRd

save_obj <- function(obj) {

  if (!is.null(obj)) {

    saveRDS(obj, obj[["cache_file_path"]])

  }

}

#' @noRd

cache_obj <- function(obj) {

  if (!is.null(obj)) {

    cache_obj <- list(
      data = obj, hash = obj[["hash"]], timeout = obj[["timeout"]]
    )

    set_cache(cache_obj)

  }

}

#' @noRd

append_obj <- function(obj) {

  if (!is.null(obj)) {

    blob <- serialize(obj, NULL)

    hash <- obj[["hash"]]

    db_cache <- data.frame(
      hash = hash,
      created = Sys.time(),
      timeout = obj[["timeout"]],
      blob = blob::blob(blob)
    )

    debug_msg("INFO [", format(Sys.time()), "] ", "Adding to cache: ", hash)

    fcp <- getOption("finbif_cache_path")

    DBI::dbAppendTable(fcp, "finbif_cache", db_cache)

  }

}

#' @noRd

get_restricted_access_token <- function(obj) {

  token <- "unset"

  restricted_api <- obj[["restricted_api"]]

  if (!is.null(restricted_api)) {

    token <- Sys.getenv(restricted_api)

    if (identical(token, "")) {

      stop("Restricted API token declared but token is unset", call. = FALSE)

    }

  }

  token

}

#' @noRd

debug_msg <- function(...) {

  debug <- Sys.getenv("FINBIF_DEBUG", "nullfile")

  debug <- switch(
    debug,
    nullfile = nullfile(),
    stdout = stdout(),
    stderr = stderr(),
    debug
  )

  cat(..., "\n", file = debug, sep = "", append = TRUE)

}
