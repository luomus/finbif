#' @noRd
#' @importFrom digest digest
#' @importFrom httr content RETRY
#' @importFrom utils packageVersion

api_get <- function(obj) {

  fb_access_token <- Sys.getenv("FINBIF_ACCESS_TOKEN")

  no_token <- identical(fb_access_token, "")

  if (no_token) {

    stop(
      "Access token for FinBIF has not been set. Use finbif_get_token() to \n",
      "have an access token sent to your email address. Then set it as the \n",
      "environment variable FINBIF_ACCESS_TOKEN with \n",
      "Sys.setenv(FINBIF_ACCESS_TOKEN = \"<access_token_sent_to_your_email>\")",
      call. = FALSE
    )

  }

  url <- getOption("finbif_api_url")

  version <- getOption("finbif_api_version")

  hash <- NULL

  cache <- obj[["cache"]]

  timeout <- cache

  cache_logical <- is.logical(cache)

  if (cache_logical) {

    timeout <- Inf

  } else {

    cache <- cache > 0

  }

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

        has_obj <- !is.null(obj)

        if (has_obj) {

          cache_obj <- list(data = obj, hash = hash, timeout = timeout)

          set_cache(cache_obj)

        }

      })

    } else if (is_path) {

      cache_file_name <- paste0("finbif_cache_file_", hash)

      cache_file_path <- file.path(fcp, cache_file_name)

      cache_file_exists <- file.exists(cache_file_path)

      if (cache_file_exists) {

        created <- file.mtime(cache_file_path)

        timeout <- timeout * 3600

        current <- Sys.time()

        elapsed <- current - created

        valid <- timeout > elapsed

        if (valid) {

          cached_obj <- readRDS(cache_file_path)

          return(cached_obj)

        } else {

          unlink(cache_file_path)

        }

      }

      on.exit({

        has_obj <- !is.null(obj)

        if (has_obj) {

          saveRDS(obj, cache_file_path)

        }

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

          timeout <- db_cache[["timeout"]]

          timeout <- timeout * 3600

          current <- Sys.time()

          elapsed <- current - created

          valid <- timeout > elapsed

          if (valid) {

            cached_obj <- db_cache[["blob"]]

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

        has_obj <- !is.null(obj)

        if (has_obj) {

          created <- Sys.time()

          blob <- serialize(obj, NULL)

          blob <- blob::blob(blob)

          db_cache <- data.frame(
            hash = hash, created = created, timeout = timeout, blob = blob
          )

          DBI::dbAppendTable(fcp, "finbif_cache", db_cache)

        }

      })

    }

  }

  allow <- getOption("finbif_allow_query")

  stopifnot("Request not cached and option:finbif_allow_query = FALSE" = allow)

  email <- getOption("finbif_email")

  has_email <- !is.null(email)

  if (has_email) {

    email_par <- list(personEmail = email)

    query <- c(query, email_par)

  }

  fb_restricted_access_token <- Sys.getenv(
    "FINBIF_RESTRICTED_ACCESS_TOKEN", "unset"
  )

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

  url_path <- sprintf("%s/%s/%s", url, version, path)

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

  query <- c(query, fb_access_token_par)

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
