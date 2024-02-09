#' Get a FinBIF personal access token
#'
#' Have a personal access token for use with the FinBIF API sent to a specified
#' email address.
#'
#' @aliases fb_request_token
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
#' }
#' @export
#' @importFrom httr content RETRY
#' @importFrom utils packageVersion

finbif_request_token <- function(email, quiet = FALSE) {

  fb_access_token <- Sys.getenv("FINBIF_ACCESS_TOKEN")

  if (identical(fb_access_token, "")) {

    allow <- getOption("finbif_allow_query")

    stopifnot("Option:finbif_allow_query = FALSE" = allow)

    url <- getOption("finbif_api_url")

    version <- getOption("finbif_api_version")

    pkg_version <- utils::packageVersion("finbif")

    agent <- paste0("https://github.com/luomus/finbif#", pkg_version)

    config <- list(
      headers = c(Accept = "application/json"),
      options =  list(useragent = agent)
    )

    resp <- httr::RETRY(
      "POST",
      sprintf("%s/%s/%s", url, version, "api-users"),
      structure(config, class = "request"),
      body = list(email = email),
      encode = "json",
      times = getOption("finbif_retry_times"),
      pause_base = getOption("finbif_retry_pause_base"),
      pause_cap = getOption("finbif_retry_pause_cap"),
      pause_min = getOption("finbif_retry_pause_min"),
      quiet = quiet,
      terminate_on = c(404L, 422L)
    )

    parsed <- httr::content(resp)

    if (!identical(resp[["status_code"]], 200L)) {

      msg <- sprintf(
        "API request failed [%s]\n%s",
        parsed[[c("error", "statusCode")]],
        parsed[[c("error", "message")]]
      )

      stop(msg, call. = FALSE)

    }

    if (!quiet) {

      message(
        "A personal access token for api.laji.fi has been sent to: ",
        email
      )

    }

    ans <- list(content = parsed, path = "api-users", response = resp)

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
