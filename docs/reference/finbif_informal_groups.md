# FinBIF informal groups

Display the informal taxonomic groups used in the FinBIF database.

## Usage

``` r
finbif_informal_groups(
  group,
  limit = 5,
  quiet = FALSE,
  locale = getOption("finbif_locale"),
  cache = getOption("finbif_use_cache_metadata")
)
```

## Arguments

- group:

  Character. Optional, if supplied only display this top-level group and
  its subgroups.

- limit:

  Integer. The maximum number top-level informal groups (and their
  sub-groups) to display.

- quiet:

  Logical. Return informal group names without displaying them.

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

A character vector (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{

# Display the informal taxonomic groups used by FinBIF
finbif_informal_groups()

} # }
```
