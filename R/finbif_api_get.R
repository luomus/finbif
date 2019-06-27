#' @importFrom httr accept_json content http_type modify_url GET user_agent
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON

finbif_api_get <- function(path, query) {
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

  resp <- httr::GET(
    httr::modify_url(url, path = path),
    httr::user_agent("https://bitbucket.org/luomus/finbif"),
    httr::accept_json(),
    query = c(query, list(access_token = finbif_access_token))
  )

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text"), simplifyVector = FALSE
  )

  if (status_code(resp) != 200) {
    stop(
      sprintf(
        "API request failed [%s]\n%s>",
        httr::status_code(resp),
        parsed$message
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "finbif_api"
  )
}
