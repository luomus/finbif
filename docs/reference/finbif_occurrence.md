# Download FinBIF occurrence records

Download filtered occurrence data from FinBIF as a `data.frame`.

## Usage

``` r
finbif_occurrence(
  ...,
  filter = NULL,
  select = NULL,
  order_by = NULL,
  aggregate = "none",
  sample = FALSE,
  n = 10,
  page = 1,
  count_only = FALSE,
  quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"),
  dwc = FALSE,
  date_time_method = NULL,
  check_taxa = TRUE,
  on_check_fail = c("warn", "error"),
  tzone = getOption("finbif_tz"),
  locale = getOption("finbif_locale"),
  seed = NULL,
  drop_na = FALSE,
  aggregate_counts = TRUE,
  exclude_na = FALSE,
  unlist = FALSE,
  facts = NULL,
  duplicates = FALSE,
  filter_col = NULL,
  restricted_api = NULL
)
```

## Arguments

- ...:

  Character vectors or list of character vectors. Taxa of records to
  download.

- filter:

  List of named character vectors. Filters to apply to records.

- select:

  Character vector. Variables to return. If not specified, a default set
  of commonly used variables will be used. Use `"default_vars"` as a
  shortcut for this set. Variables can be deselected by prepending a `-`
  to the variable name. If only deselects are specified the default set
  of variables without the deselection will be returned.

- order_by:

  Character vector. Variables to order records by before they are
  returned. Most, though not all, variables can be used to order records
  before they are returned. Ordering is ascending by default. To return
  in descending order append a `-` to the front of the variable (e.g.,
  `"-date_start"`). Default order is `"-date_start"` \> `"-load_date"`
  \> `"reported_name"` \> `"record_id"`.

- aggregate:

  Character. If `"none"` (default), returns full records. If one or more
  of `"records"`, `"species"`, `"taxa"`, `"individuals"`, `"pairs"`,
  `"events"` or `"documents"`; aggregates combinations of the selected
  variables by counting records, species, taxa, individuals or events or
  documents. Aggregation by events or documents cannot be done in
  combination with any of the other aggregation types.

- sample:

  Logical. If `TRUE` randomly sample the records from the FinBIF
  database.

- n:

  Integer. How many records to download/import.

- page:

  Integer. Which page of records to start downloading from.

- count_only:

  Logical. Only return the number of records available.

- quiet:

  Logical. Suppress the progress indicator for multipage downloads.
  Defaults to value of option `finbif_hide_progress`.

- cache:

  Logical or Integer. If `TRUE` or a number greater than zero, then
  data-caching will be used. If not logical then the cache will be
  invalidated after the number of hours indicated by the argument. If a
  length one vector is used, its value will only apply to caching
  occurrence records. If the value is length two, then the second
  element will determine how metadata is cached.

- dwc:

  Logical. Use Darwin Core (or Darwin Core style) variable names.

- date_time_method:

  Character. Passed to
  [`lutz::tz_lookup_coords()`](http://andyteucher.ca/lutz/reference/tz_lookup_coords.md)
  when `date_time` and/or `duration` variables have been selected.
  Default is `"fast"` when less than 100,000 records are requested and
  `"none"` when more. Using method `"none"` assumes all records are in
  timezone "Europe/Helsinki", Use `date_time_method = "accurate"`
  (requires package `sf`) for greater accuracy at the cost of slower
  computation.

- check_taxa:

  Logical. Check first that taxa are in the FinBIF database. If true
  only records that match known taxa (have a valid taxon ID) are
  returned.

- on_check_fail:

  Character. What to do if a taxon is found not valid. One of `"warn"`
  (default) or `"error"`.

- tzone:

  Character. If `date_time` has been selected the timezone of the
  outputted date-time. Defaults to system timezone.

- locale:

  Character. One of the supported two-letter ISO 639-1 language codes.
  Current supported languages are English, Finnish and Swedish. For data
  where more than one language is available the language denoted by
  `locale` will be preferred while falling back to the other languages
  in the order indicated above.

- seed:

  Integer. Set a seed for randomly sampling records.

- drop_na:

  Logical. A vector indicating which columns to check for missing data.
  Values recycled to the number of columns. Defaults to all columns.

- aggregate_counts:

  Logical. Should count variables be returned when using aggregation.

- exclude_na:

  Logical. Should records where all selected variables have non-NA
  values only be returned.

- unlist:

  Logical. Should variables that contain non atomic data be concatenated
  into a string separated by ";"?

- facts:

  Character vector. Extra variables to be extracted from record, event
  and document "facts".

- duplicates:

  Logical. If `TRUE`, allow duplicate records/aggregated records when
  making multi-filter set requests. If `FALSE` (default) duplicate
  records are removed.

- filter_col:

  Character. The name of a column, with values derived from the names of
  the filter sets used when using multiple filters, to include when
  using multiple filter sets. If `NULL` (default), no column is
  included.

- restricted_api:

  Character. If using a restricted data API token in addition to a
  personal access token, a string indicating the name of an environment
  variable storing the restricted data API token.

## Value

A `data.frame`. If `count_only = TRUE` an integer.

## Examples

``` r
if (FALSE) { # \dontrun{

# Get recent occurrence data for taxon
finbif_occurrence("Cygnus cygnus")

# Specify the number of records
finbif_occurrence("Cygnus cygnus", n = 100)

# Get multiple taxa
finbif_occurrence("Cygnus cygnus", "Ursus arctos")

# Filter the records
finbif_occurrence(
  species = "Cygnus cygnus",
  filter = list(coordinate_accuracy_max = 100)
)

} # }
```
