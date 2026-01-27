# FinBIF metadata

Display metadata from the FinBIF database.

## Usage

``` r
finbif_metadata(
  which,
  locale = getOption("finbif_locale"),
  cache = getOption("finbif_use_cache_metadata")
)
```

## Arguments

- which:

  Character. Which category of metadata to display. If unspecified,
  function returns the categories of metadata available.

- locale:

  Character. One of the supported two-letter ISO 639-1 language codes.
  Current supported languages are English, Finnish and Swedish. For data
  where more than one language is available the language denoted by
  `locale` will be preferred while falling back to the other languages
  in the order indicated above.

- cache:

  Logical or Integer. If `TRUE` or a number greater than zero, then
  data-caching will be used. If not logical then cache will be
  invalidated after the number of hours indicated by the argument.

## Value

A data.frame.

## Examples

``` r
if (FALSE) { # \dontrun{

finbif_metadata("red_list")

} # }
```
