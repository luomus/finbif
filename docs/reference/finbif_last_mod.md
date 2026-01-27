# Get last modified date for FinBIF occurrence records

Get last modified date for filtered occurrence data from FinBIF.

## Usage

``` r
finbif_last_mod(..., filter)
```

## Arguments

- ...:

  Character vectors or list of character vectors. Taxa of records to
  download.

- filter:

  List of named character vectors. Filters to apply to records.

## Value

A `Date` object

## Examples

``` r
if (FALSE) { # \dontrun{

# Get last modified date for Whooper Swan occurrence records from Finland
finbif_last_mod("Cygnus cygnus", filter = c(country = "Finland"))

} # }
```
