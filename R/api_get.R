#' @noRd
#' @importFrom digest digest
#' @importFrom httr accept_json content GET http_type user_agent status_code
#' @importFrom jsonlite fromJSON
#' @importFrom utils packageVersion

api_get <- function(path, query, cache) {
  finbif_access_token <- token()

  if (is.null(finbif_access_token)) {
    stop(
      "Access token for FinBIF has not been set. Use finbif_get_token() to",
      "have an access token sent to your email address. Then set it as the",
      "environment variable FINBIF_ACCESS_TOKEN with",
      "Sys.setenv(FINBIF_ACCESS_TOKEN = \"<access_token_sent_to_your_email>\")",
      call. = FALSE
    )
  }

  url <- getOption("finbif_api_url")
  version <- getOption("finbif_api_version")

  if (cache) {
    hash <- digest::digest(list(url, version, path, query))
    fcp <- getOption("finbif_cache_path")
    if (is.null(fcp)) {
      ans <- get_cache(hash)
      if (!is.null(ans)) return(ans)
      on.exit(if (!is.null(ans)) set_cache(ans, hash))
    } else {
      cache_file <- file.path(fcp, paste0("finbif_cache_file_", hash))
      if (file.exists(cache_file)) return(readRDS(cache_file))
      on.exit(if (!is.null(ans)) saveRDS(ans, cache_file))
    }
  }

  resp <- httr::GET(
    sprintf("https://%s/%s/%s", url, version, path),
    httr::user_agent(
      paste0(
        "https://github.com/luomus/finbif#",
        utils::packageVersion("finbif")
      )
    ),
    httr::accept_json(),
    query = c(query, list(access_token = finbif_access_token))
  )

  notoken <-
    sub(sprintf("&access_token=%s", finbif_access_token), "", resp[["url"]])

  resp[["request"]][["url"]] <- resp[["url"]] <- notoken

  if (httr::http_type(resp) != "application/json") {
    ans <- NULL
    stop("API did not return json", call. = FALSE)
  }

  parsed <-
    jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  if (httr::status_code(resp) != 200) {
    ans <- NULL
    stop(
      sprintf(
        "API request failed [%s]\n%s>",
        httr::status_code(resp),
        parsed[["message"]]
      ),
      call. = FALSE
    )
  }

  ans <- structure(
    list(
      content = parsed, path = path, response = resp,
      hash = if (exists("hash")) hash
    ),
    class = "finbif_api"
  )

  ans
}
