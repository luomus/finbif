#' Get a FinBIF personal access token
#'
#' Have a personal access token for use with the FinBIF API sent to a specified
#' email address
#'
#' @param email Character. The email address to which to send the API access
#'   token.
#' @return If an access token has already been set then `NULL` (invisibly) if
#'   not then, invisbly, a `finbif_api` object.
#' @importFrom httr accept_json content http_type modify_url POST user_agent
#' @importFrom httr status_code
#' @importFrom jsonlite fromJSON

finbif_get_token <- function(email) {
  finbif_access_token <- finbif_token()

  if (!is.null(finbif_access_token)) {
    message(
      "An access token has already been set. If you want to set receive a \n",
      "new token, first remove the current token with \n",
      "Sys.unsetenv(\"FINBIF_ACCESS_TOKEN\")"
    )
    return(invisible(NULL))
  }

  url <- "https://api.laji.fi"
  path <- "v0/api-users"
  resp <- httr::POST(
    httr::modify_url(url, path = path),
    httr::user_agent("https://bitbucket.org/luomus/finbif"),
    httr::accept_json(),
    body = list(email = email),
    encode = "json"
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
        "API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  } else {
    message("A personal access token for api.laji.fi has been sent to: ", email)
  }

  ans <- structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class("finbif_api")
  )

  invisible(ans)
}
