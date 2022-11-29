#' @noRd
#' @importFrom digest digest
#' @importFrom httr accept_json content RETRY http_type user_agent status_code
#' @importFrom utils packageVersion

api_get <- function(obj) {

  finbif_access_token <- token()

  if (is.null(finbif_access_token)) {
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

  if (obj[["cache"]]) {

    hash <- digest::digest(
      list(
        sub(".*://", "", url),
        version,
        obj[["path"]],
        obj[["query"]]
      )
    )

    fcp <- getOption("finbif_cache_path")

    if (is.null(fcp)) {

      cached_obj <- get_cache(hash)

      if (!is.null(cached_obj)) return(cached_obj)

      on.exit(if (!is.null(obj)) set_cache(list(data = obj, hash = hash)))

    } else {

      cache_file <- file.path(fcp, paste0("finbif_cache_file_", hash))

      if (file.exists(cache_file)) return(readRDS(cache_file))

      on.exit(if (!is.null(obj)) saveRDS(obj, cache_file))

    }
  }

  stopifnot(
    "Request not cached and option:finbif_allow_query = FALSE" =
      getOption("finbif_allow_query")
  )

  email <- getOption("finbif_email")

  if (!is.null(email)) {

    obj[["query"]] <- c(obj[["query"]], list(personEmail = email))

  }

  finbif_restricted_access_token <- Sys.getenv(
    "FINBIF_RESTRICTED_ACCESS_TOKEN", "unset"
  )

  obj[["query"]] <- switch(
    finbif_restricted_access_token,
    unset = obj[["query"]],
    c(obj[["query"]], list(permissionToken = finbif_restricted_access_token))
  )

  # Pausing between requests is important if many request will be made
  Sys.sleep(1 / getOption("finbif_rate_limit"))

  agent <- paste0(
    "https://github.com/luomus/finbif#", utils::packageVersion("finbif")
  )

  agent <- Sys.getenv("FINBIF_USER_AGENT", agent)

  resp <- httr::RETRY(
    "GET",
    sprintf("%s/%s/%s", url, version, obj[["path"]]),
    httr::user_agent(paste0(agent, ":", get_calling_function("finbif"))),
    httr::accept_json(),
    query = c(obj[["query"]], list(access_token = finbif_access_token)),
    times = getOption("finbif_retry_times"),
    pause_base = getOption("finbif_retry_pause_base"),
    pause_cap = getOption("finbif_retry_pause_cap"),
    pause_min = getOption("finbif_retry_pause_min"),
    terminate_on = 404L
  )

  notoken <- sub(
    paste0("&access_token=", finbif_access_token), "", resp[["url"]]
  )

  notoken <- sub(paste0("&personEmail=", email), "", notoken)

  notoken <- sub(
    paste0("&permissionToken=", finbif_restricted_access_token), "", notoken
  )

  resp[["url"]] <- notoken
  resp[["request"]][["url"]] <- notoken

  if (httr::http_type(resp) != "application/json") {

    obj <- NULL

    stop("API did not return json", call. = FALSE)

  }

  parsed <- httr::content(resp)

  if (httr::status_code(resp) != 200L) {

    obj <- NULL

    stop(
      sprintf(
        "API request failed [%s]\n%s>",
        httr::status_code(resp),
        parsed[["message"]]
      ),
      call. = FALSE
    )
  }

  obj[["content"]] <- parsed
  obj[["resp"]] <- resp
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
