#' @noRd
#' @importFrom digest digest
#' @importFrom httr accept_json content http_type modify_url GET user_agent
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON
#' @importFrom urltools param_remove

finbif_api_get <- function(path, query, cache) {
  finbif_access_token <- finbif_token()

  if (is.null(finbif_access_token)) {
    stop(
      "Access token for FinBIF has not been set. Use finbif_get_token() to",
      "have an access token sent to your email address. Then set it as the",
      "environment variable FINBIF_ACCESS_TOKEN with",
      "Sys.setenv(FINBIF_ACCESS_TOKEN = \"<access_token_sent_to_your_email>\")",
      call. = FALSE
    )
  }

  url <- "https://api.laji.fi"

  if (getOption("finbif_use_cache") && cache) {
    hash <- digest::digest(list(path, query))
    fcp <- getOption("finbif_cache_path")
    fcp <- if (is.null(fcp)) tempdir()
    cache_file <- file.path(fcp, paste0("finbif_cache_file_", hash))
    if (file.exists(cache_file)) return(readRDS(cache_file))
    on.exit(if (!is.null(ans)) saveRDS(ans, cache_file))
  }

  resp <- httr::GET(
    httr::modify_url(url, path = path),
    httr::user_agent("https://bitbucket.org/luomus/finbif"),
    httr::accept_json(),
    query = c(query, list(access_token = finbif_access_token))
  )

  notoken <- urltools::param_remove(resp[["url"]], "access_token")

  resp[["request"]][["url"]] <- resp[["url"]] <- notoken

  if (httr::http_type(resp) != "application/json") {
    ans <- NULL
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text"), simplifyVector = FALSE
  )

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
      content = parsed,
      path = path,
      response = resp
    ),
    class = "finbif_api"
  )

  ans
}
