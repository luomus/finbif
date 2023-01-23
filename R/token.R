#' @noRd

token <- function(
  quiet = TRUE
) {

  finbif_access_token <- Sys.getenv("FINBIF_ACCESS_TOKEN")

  if (!quiet) {

    message(
      "Using FinBIF access token from environment variable FINBIF_ACCESS_TOKEN"
    )

  }

  no_token <- identical(finbif_access_token, "")

  if (no_token) {

    finbif_access_token <- NULL

  }

  finbif_access_token

}
