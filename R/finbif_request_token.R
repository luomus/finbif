#' Get a FinBIF personal access token
#'
#' Have a personal access token for use with the FinBIF API sent to a specified
#' email address.
#'
#' @aliases fb_request_token
#'
#' @param email Character. The email address to which to send the API access
#'   token.
#' @return If an access token has already been set then `NULL` (invisibly) if
#'   not then, invisibly, a `finbif_api` object containing the response from
#'   the FinBIF server.
#' @importFrom httr accept_json content http_type RETRY user_agent status_code
#' @importFrom jsonlite fromJSON
#' @importFrom utils packageVersion
#' @examples \dontrun{
#'
#' # Request a token for example@email.com
#' finbif_request_token("example@email.com")
#' }
#' @export

finbif_request_token <- function(email) {
  finbif_access_token <- token()

  if (!is.null(finbif_access_token)) {
    message(
      "An access token has already been set. If you want to receive a new \n",
      "token, first remove the current token with \n",
      "Sys.unsetenv(\"FINBIF_ACCESS_TOKEN\")"
    )
    return(invisible(NULL))
  }

  url     <- getOption("finbif_api_url")
  version <- getOption("finbif_api_version")
  path    <- "api-users"
  resp <- httr::RETRY(
    verb = "POST",
    url = sprintf("https://%s/%s/%s", url, version, path),
    config = httr::user_agent(
      paste0(
        "https://github.com/luomus/finbif#",
        utils::packageVersion("finbif")
      )
    ),
    httr::accept_json(),
    body = list(email = email),
    encode = "json"
  )

  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text"), simplifyVector = FALSE
  )

  status <- httr::status_code(resp)

  if (!identical(status, 200L)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s",
        status,
        parsed[["message"]]
      ),
      call. = FALSE
    )
  } else {
    message("A personal access token for api.laji.fi has been sent to: ", email)
  }

  ans <- structure(
    list(content = parsed, path = path, response = resp),
    class = "finbif_api"
  )

  invisible(ans)

}
