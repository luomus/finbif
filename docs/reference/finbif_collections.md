# FinBIF collections

Get information on collections in the FinBIF database.

## Usage

``` r
finbif_collections(
  filter,
  select,
  subcollections = TRUE,
  supercollections = FALSE,
  locale = getOption("finbif_locale"),
  nmin = 0,
  cache = getOption("finbif_use_cache_metadata")
)
```

## Arguments

- filter:

  Logical. Expression indicating elements or rows to keep: missing
  values are taken as false.

- select:

  Expression. Indicates columns to select from the data frame.

- subcollections:

  Logical. Return subcollection metadata of higher level collections.

- supercollections:

  Logical. Return lowest level collection metadata.

- locale:

  Character. Language of data returned. One of "en", "fi", or "sv".

- nmin:

  Integer. Filter collections by number of records. Only return
  information on collections with greater than value specified. If `NA`
  then return information on all collections.

- cache:

  Logical or Integer. If `TRUE` or a number greater than zero, then
  data-caching will be used. If not logical then cache will be
  invalidated after the number of hours indicated by the argument.

## Value

A data.frame.

## Examples

``` r
if (FALSE) { # \dontrun{

# Get collection metadata
collections <- finbif_collections()

} # }
```
