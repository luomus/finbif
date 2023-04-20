#' @noRd
#' @importFrom digest digest
#' @importFrom httr content RETRY
#' @importFrom utils packageVersion

api_get <- function(obj) {

  fb_access_token <- get_token()

  fb_restricted_access_token <- get_restricted_access_token(obj)

  url <- getOption("finbif_api_url")

  version <- getOption("finbif_api_version")

  hash <- NULL

  cache <- cache_as_logical(obj)

  timeout <- get_timeout(obj)

  obj[["timeout"]] <- timeout

  query <- obj[["query"]]

  path <- obj[["path"]]

  if (cache) {

    query_list <- list(url, version, path, query)

    hash <- digest::digest(query_list)

    fcp <- getOption("finbif_cache_path")

    is_path <- is.character(fcp)

    no_cache_path <- is.null(fcp)

    if (no_cache_path) {

      cached_obj <- get_cache(hash)

      has_cached_obj <- !is.null(cached_obj)

      if (has_cached_obj) {

        return(cached_obj)

      }

      on.exit({

        cache_obj(obj)

      })

    } else if (is_path) {

      cache_file_name <- paste0("finbif_cache_file_", hash)

      cache_file_path <- file.path(fcp, cache_file_name)

      obj[["cache_file_path"]] <- cache_file_path

      cache_file_exists <- file.exists(cache_file_path)

      if (cache_file_exists) {

        created <- file.mtime(cache_file_path)

        valid <- cache_is_valid(timeout, created)

        if (valid) {

          cached_obj <- readRDS(cache_file_path)

          return(cached_obj)

        } else {

          unlink(cache_file_path)

        }

      }

      on.exit({

        save_obj(obj)

      })

    } else {

      has_dbi <- has_pkgs("DBI", "blob")

      stopifnot("Packages {DBI} & {blob} needed to use a DB cache" = has_dbi)

      has_table <- DBI::dbExistsTable(fcp, "finbif_cache")

      if (!has_table) {

        created <- numeric()

        created <- as.POSIXct(created)

        timeout_init <- numeric()

        blob <- blob::blob()

        init <- data.frame(
          hash = character(),
          created = created,
          timeout = timeout_init,
          blob = blob
        )

        DBI::dbWriteTable(fcp, "finbif_cache", init)

      } else {

        db_query <- "SELECT * FROM finbif_cache WHERE hash = '%s'"

        db_query <- sprintf(db_query, hash)

        db_cache <- DBI::dbGetQuery(fcp, db_query)

        nrows <- nrow(db_cache)

        has_cached_obj <- nrows > 0L

        if (has_cached_obj) {

          created <- db_cache[["created"]]

          created <- as.POSIXct(created, origin = "1970-01-01")

          last_cache_ind <- which.max(created)

          last_cache_ind <- last_cache_ind[[1L]]

          created <- created[[last_cache_ind]]

          timeout <- db_cache["timeout"]

          timeout <- timeout[[last_cache_ind]]

          valid <- cache_is_valid(timeout, created)

          if (valid) {

            cached_obj <- db_cache[last_cache_ind, "blob"]

            cached_obj <- cached_obj[[1L]]

            cached_obj <- unserialize(cached_obj)

            return(cached_obj)

          } else {

            db_query <- "DELETE FROM finbif_cache WHERE hash = '%s'"

            db_query <- sprintf(db_query, hash)

            DBI::dbExecute(fcp, db_query)

          }

        }

      }

      on.exit({

        append_obj(obj)

      })

    }

  }

  allow <- getOption("finbif_allow_query")

  stopifnot("Request not cached and option:finbif_allow_query = FALSE" = allow)

  query <- add_email(query)

  fb_restricted_access_token_par <- list(
    permissionToken = fb_restricted_access_token
  )

  query_w_fb_restricted_access <- c(query, fb_restricted_access_token_par)

  query <- switch(
    fb_restricted_access_token, unset = query, query_w_fb_restricted_access
  )

  # Pausing between requests is important if many request will be made
  rate_limit <- getOption("finbif_rate_limit")

  sleep <- 1 / rate_limit

  Sys.sleep(sleep)

  private_api <- Sys.getenv("FINBIF_PRIVATE_API", "unset")

  private_api <- switch(
    private_api,
    unset = "laji.fi",
    sprintf("%s.%s", private_api, "laji.fi")
  )

  use_private_api <- Sys.getenv("FINBIF_USE_PRIVATE_API")

  use_private_api <- tolower(use_private_api)

  url_path <- switch(
    use_private_api,
    true = sprintf("https://%s/api/%s", private_api, path),
    sprintf("%s/%s/%s", url, version, path)
  )

  pkg_version <- utils::packageVersion("finbif")

  calling_fun <- get_calling_function("finbif")

  agent <- paste0("https://github.com/luomus/finbif#", pkg_version)

  agent <- paste0(agent, ":", calling_fun)

  agent <- Sys.getenv("FINBIF_USER_AGENT", agent)

  agent <- list(useragent = agent)

  accept <- c(Accept = "application/json")

  config <- list(headers = accept, options = agent)

  config <- structure(config, class = "request")

  fb_access_token_par <- list(access_token = fb_access_token)

  query <- switch(use_private_api, true = query, c(query, fb_access_token_par))

  times <- getOption("finbif_retry_times")

  pause_base <- getOption("finbif_retry_pause_base")

  pause_cap <- getOption("finbif_retry_pause_cap")

  pause_min <- getOption("finbif_retry_pause_min")

  resp <- httr::RETRY(
    "GET",
    url_path,
    config,
    query = query,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap,
    pause_min = pause_min,
    terminate_on = 404L
  )

  resp_url <- resp[["url"]]

  fb_access_token_str <- paste0("&access_token=", fb_access_token)

  notoken <- gsub(fb_access_token_str, "", resp_url)

  email <- getOption("finbif_email")

  email_str <- paste0("&personEmail=", email)

  notoken <- gsub(email_str, "", notoken)

  fb_restricted_access_token_str <- paste0(
    "&permissionToken=", fb_restricted_access_token
  )

  notoken <- gsub(fb_restricted_access_token_str, "", notoken)

  resp[["url"]] <- notoken

  request_url <- c("request", "url")

  resp[[request_url]] <- notoken

  content_type <- c("headers", "content-type")

  resp_type <- resp[[content_type]]

  resp_type <- gsub("\\s", "", resp_type)

  resp_not_json <- !identical(resp_type, "application/json;charset=utf-8")

  if (resp_not_json) {

    obj <- NULL

    stop("API did not return json", call. = FALSE)

  }

  parsed <- httr::content(resp)

  status_code <- resp[["status_code"]]

  status_code_error <- !identical(status_code, 200L)

  if (status_code_error) {

    obj <- NULL

    err_msg_body <- parsed[["message"]]

    err_msg <- sprintf(
      "API request failed [%s]\n%s>", status_code, err_msg_body
    )

    stop(err_msg, call. = FALSE)

  }

  obj[["content"]] <- parsed

  obj[["response"]] <- resp

  obj[["hash"]] <- hash

  structure(obj, class = "finbif_api")

}

#' @noRd

get_calling_function <- function(pkg) {

  calls <- sys.calls()

  for (call in calls) {

    fun <- try({

        f <- call[[1L]]

        as.character(f)

      },
      silent = TRUE
    )

    no_error <- inherits(fun, "character")

    if (no_error) {

      len <- length(fun)

      fun <- fun[[len]]

      ns <- getNamespace(pkg)

      nms <- ls(ns)

      in_ns <- fun %in% nms

      if (in_ns) {

        break

      }

    }

  }

  args <- call[-1L]

  n_args <- length(args)

  has_args <- n_args > 0L

  arg_nm_strs <- ""

  if (has_args) {

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

  has_email <- !is.null(email)

  use_private_api <- Sys.getenv("FINBIF_USE_PRIVATE_API")

  use_private_api <- as.logical(use_private_api)

  use_private_api <- isTRUE(use_private_api)

  add_email <- has_email && !use_private_api

  if (add_email) {

    email_par <- list(personEmail = email)

    query <- c(query, email_par)

  }

  query

}

#' @noRd

get_token <- function() {

  fb_access_token <- Sys.getenv("FINBIF_ACCESS_TOKEN")

  no_token <- identical(fb_access_token, "")

  use_private_api <- Sys.getenv("FINBIF_USE_PRIVATE_API")

  use_private_api <- as.logical(use_private_api)

  use_private_api <- isTRUE(use_private_api)

  needs_token <- no_token && !use_private_api

  if (needs_token) {

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

cache_as_logical <- function(obj) {

  cache <- obj[["cache"]]

  cache > 0

}

#' @noRd

get_timeout <- function(obj) {

  timeout <- obj[["cache"]]

  is_logical <- is.logical(timeout)

  if (is_logical) {

    timeout <- Inf

  }

  timeout

}

#' @noRd

save_obj <- function(obj) {

  has_obj <- !is.null(obj)

  if (has_obj) {

    cache_file_path <- obj[["cache_file_path"]]

    saveRDS(obj, cache_file_path)

  }

}

#' @noRd

cache_obj <- function(obj) {

  has_obj <- !is.null(obj)

  if (has_obj) {

    hash <- obj[["hash"]]

    timeout <- obj[["timeout"]]

    cache_obj <- list(data = obj, hash = hash, timeout = timeout)

    set_cache(cache_obj)

  }

}

#' @noRd

append_obj <- function(obj) {

  has_obj <- !is.null(obj)

  if (has_obj) {

    fcp <- getOption("finbif_cache_path")

    hash <- obj[["hash"]]

    created <- Sys.time()

    timeout <- obj[["timeout"]]

    blob <- serialize(obj, NULL)

    blob <- blob::blob(blob)

    db_cache <- data.frame(
      hash = hash, created = created, timeout = timeout, blob = blob
    )

    DBI::dbAppendTable(fcp, "finbif_cache", db_cache)

  }

}

get_restricted_access_token <- function(obj) {

  token <- "unset"

  restricted_api <- obj[["restricted_api"]]

  has_token <- !is.null(restricted_api)

  if (has_token) {

    token <- Sys.getenv(restricted_api)

    token_empty <- identical(token, "")

    if (token_empty) {

      stop("Restricted API token declared but token is unset", .call = FALSE)

    }

  }

  token

}
