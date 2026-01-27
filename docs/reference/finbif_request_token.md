# Get a FinBIF personal access token

Have a personal access token for use with the FinBIF API sent to a
specified email address.

## Usage

``` r
finbif_request_token(email, quiet = FALSE)

finbif_renew_token(email, quiet = FALSE)
```

## Arguments

- email:

  Character. The email address to which to send the API access token.

- quiet:

  Logical. Suppress messages.

## Value

If an access token has already been set then `NULL` (invisibly) if not
then, invisibly, a `finbif_api` object containing the response from the
FinBIF server.

## Examples

``` r
if (FALSE) { # \dontrun{

# Request a token for example@email.com
finbif_request_token("example@email.com")

Sys.unsetenv("FINBIF_ACCESS_TOKEN")

finbif_renew_token("example@email.com")

} # }
```
