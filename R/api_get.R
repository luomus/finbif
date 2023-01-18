#' @noRd
#' @importFrom digest digest
#' @importFrom httr accept_json content RETRY http_type user_agent status_code
#' @importFrom utils packageVersion

api_get <- function(obj) {

  finbif_access_token <- token()

  finbif_access_token_null <- is.null(finbif_access_token)

  if (finbif_access_token_null) {

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

  query <- obj[["query"]]

  path <- obj[["path"]]

  if (cache) {

    url_sans_protocol <- sub(".*://", "", url)

    query_list <- list(url_sans_protocol, version, path, query)

    hash <- digest::digest(query_list)

    fcp <- getOption("finbif_cache_path")

    if (is.null(fcp)) {

      cached_obj <- get_cache(hash)

      has_cached_obj <- !is.null(cached_obj)

      if (has_cached_obj) {

        return(cached_obj)

      }

      on.exit({

        has_obj <- !is.null(obj)

        if (has_obj) {

          cache_object <- list(data = obj, hash = hash)

          set_cache(cache_object)

        }

      })

    } else {

      cache_file_name <- paste0("finbif_cache_file_", hash)

      cache_file_path <- file.path(fcp, cache_file_name)

      cache_file_exists <- file.exists(cache_file_path)

      if (cache_file_exists) {

        cached_obj <- readRDS(cache_file_path)

        return(cached_obj)

      }

      on.exit({

        has_obj <- !is.null(obj)

        if (has_obj) {

          saveRDS(obj, cache_file_path)

        }

      })

    }
  }

  stopifnot(
    "Request not cached and option:finbif_allow_query = FALSE" =
      getOption("finbif_allow_query")
  )

  email <- getOption("finbif_email")

  has_email <- !is.null(email)

  if (has_email) {

    email_par <- list(personEmail = email)

    query <- c(query, email_par)

  }

  finbif_restricted_access_token <- Sys.getenv(
    "FINBIF_RESTRICTED_ACCESS_TOKEN", "unset"
  )

  finbif_restricted_access_token_par <- list(
    permissionToken = finbif_restricted_access_token
  )

  query_with_finbif_restricted_access_token <- c(
    query, finbif_restricted_access_token_par
  )

  query <- switch(
    finbif_restricted_access_token,
    unset = query,
    query_with_finbif_restricted_access_token
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

  agent <- httr::user_agent(agent)

  accept <- httr::accept_json()

  finbif_access_token_par <- list(access_token = finbif_access_token)

  query <- c(query, finbif_access_token_par)

  times <- getOption("finbif_retry_times")

  pause_base <- getOption("finbif_retry_pause_base")

  pause_cap <- getOption("finbif_retry_pause_cap")

  pause_min <- getOption("finbif_retry_pause_min")

  resp <- httr::RETRY(
    verb = "GET",
    url = url_path,
    agent,
    accept,
    query = query,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap,
    pause_min = pause_min,
    terminate_on = 404L
  )

  resp_url <- resp[["url"]]

  finbif_access_token_str <- paste0("&access_token=", finbif_access_token)

  notoken <- gsub(finbif_access_token_str, "", resp_url)

  email_str <- paste0("&personEmail=", email)

  notoken <- gsub(email_str, "", notoken)

  finbif_restricted_access_token_str <- paste0(
    "&permissionToken=", finbif_restricted_access_token
  )

  notoken <- gsub(finbif_restricted_access_token_str, "", notoken)

  resp[["url"]] <- notoken

  resp[["request"]][["url"]] <- notoken

  resp_type <- httr::http_type(resp)

  resp_not_json <- !identical(resp_type, "application/json")

  if (resp_not_json) {

    obj <- NULL

    stop("API did not return json", call. = FALSE)

  }

  parsed <- httr::content(resp)

  status_code <- httr::status_code(resp)

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

get_calling_function <- function(pkg) {

  for (call in sys.calls()) {
    fun <- try(as.character(call[[1L]]), silent = TRUE)
    if (inherits(fun, "character")) {
      fun <- fun[[length(fun)]]
      if (fun %in% ls(getNamespace(pkg))) break
    }
  }

  args <- names(call)[-1L]

  if (!length(args)) {

    values <- ""

  } else {

    values <- call[-1L]
    type   <- vapply(values, typeof, character(1L))
    len    <- vapply(values, length, integer(1L))
    values <- paste0(args, "=", type, "<", len, ">")

  }

  paste0(fun, "(", paste(values, collapse = ","), ")")

}
