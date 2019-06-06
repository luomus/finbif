#' @noRd

finbif_token <- function(quiet = TRUE) {
  finbif_access_token <- Sys.getenv("FINBIF_ACCESS_TOKEN")
  if (!quiet) {
    message(
      "Using FinBIF access token from environment variable FINBIF_ACCESS_TOKEN"
    )
  }
  if (identical(finbif_access_token, "")) return(NULL)
  finbif_access_token
}
