#' Get a FinBIF personal access token
#'
#' Have a personal access token for use with the FinBIF API sent to a specified
#' email address.
#'
#' @aliases fb_request_token fb_renew_token
#'
#' @param email Character. The email address to which to send the API access
#'   token.
#' @param quiet Logical. Suppress messages.
#' @return If an access token has already been set then `NULL` (invisibly) if
#'   not then, invisibly, a `finbif_api` object containing the response from
#'   the FinBIF server.
#' @examples \dontrun{
#'
#' # Request a token for example@email.com
#' finbif_request_token("example@email.com")
#'
#' Sys.unsetenv("FINBIF_ACCESS_TOKEN")
#'
#' finbif_renew_token("example@email.com")
#'
#' }
#' @export
finbif_request_token <- function(email, quiet = FALSE) {

  token(email, quiet, path = "api-users")

}

#' @export
#' @rdname finbif_request_token
finbif_renew_token <- function(email, quiet = FALSE) {

  token(email, quiet, path = "api-users/renew")

}

#' @importFrom httr2 req_error req_headers req_perform req_retry request
#' @importFrom httr2 req_url_query req_user_agent resp_body_json
token <- function(email, quiet = FALSE, path) {
  fb_access_token <- Sys.getenv("FINBIF_ACCESS_TOKEN")

  if (identical(fb_access_token, "")) {
    allow <- getOption("finbif_allow_query")
    stopifnot("Option: finbif_allow_query = FALSE" = allow)

    url <- getOption("finbif_api_url")

    req <- httr2::request(sprintf("%s/%s", url, path))

    req <- httr2::req_body_json(req, list(email = email))

    pkg_version <- utils::packageVersion("finbif")
    agent <- paste0("https://github.com/luomus/finbif#", pkg_version)

    req <- httr2::req_user_agent(req, Sys.getenv("FINBIF_USER_AGENT", agent))

    req <- httr2::req_headers(req, Accept = "application/json")
    req <- httr2::req_headers(
      req, `API-version` = getOption("finbif_api_version")
    )

    pause_base <- getOption("finbif_retry_pause_base")
    pause_cap <- getOption("finbif_retry_pause_cap")
    pause_min <- getOption("finbif_retry_pause_min")

    req <- httr2::req_retry(
      req,
      max_tries = getOption("finbif_retry_times"),
      backoff = function(x) pmax(pause_min, pmin(pause_cap, pause_base^x))
    )

    req <- httr2::req_error(req, is_error = function(resp) FALSE)

    resp <- httr2::req_perform(req)

    check_status(resp)

    if (!quiet) {
      message(
        "A personal access token for api.laji.fi has been sent to: ", email
      )
    }

    ans <- list(
      content = httr2::resp_body_json(resp), path = "api-users", response = resp
    )
    ans <- structure(ans, class = "finbif_api")

  } else {
    message(
      "An access token has already been set. If you want to receive a new \n",
      "token, first remove the current token with \n",
      "Sys.unsetenv(\"FINBIF_ACCESS_TOKEN\")"
    )
    ans <- NULL
  }

  invisible(ans)
}
