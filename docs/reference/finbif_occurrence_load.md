# Load FinBIF occurrence records from a file

Load occurrence data from a file as a `data.frame`.

## Usage

``` r
finbif_occurrence_load(
  file,
  select = NULL,
  n = -1,
  count_only = FALSE,
  quiet = getOption("finbif_hide_progress"),
  cache = getOption("finbif_use_cache"),
  dwc = getOption("finbif_use_dwc"),
  date_time_method = NULL,
  tzone = getOption("finbif_tz"),
  write_file = tempfile(),
  dt = NA,
  keep_tsv = FALSE,
  facts = list(),
  type_convert_facts = TRUE,
  drop_na = FALSE,
  drop_facts_na = drop_na,
  locale = getOption("finbif_locale"),
  skip = 0
)
```

## Arguments

- file:

  Character or Integer. Either the path to a Zip archive or tabular data
  file that has been downloaded from "laji.fi", a URI linking to such a
  data file (e.g., <https://tun.fi/HBF.49381>) or an integer
  representing the URI (i.e., `49381`).

- select:

  Character vector. Variables to return. If not specified, a default set
  of commonly used variables will be used. Use `"default_vars"` as a
  shortcut for this set. Variables can be deselected by prepending a `-`
  to the variable name. If only deselects are specified the default set
  of variables without the deselection will be returned. Use `"all"` to
  select all available variables in the file.

- n:

  Integer. How many records to import. Negative and other invalid values
  are ignored causing all records to be imported.

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

- tzone:

  Character. If `date_time` has been selected the timezone of the
  outputted date-time. Defaults to system timezone.

- write_file:

  Character. Path to write downloaded zip file to if `file` refers to a
  URI. Will be ignored if `getOption("finbif_cache_path")` is not `NULL`
  and will use the cache path instead.

- dt:

  Logical. If package, `data.table`, is available return a `data.table`
  object rather than a `data.frame`.

- keep_tsv:

  Logical. Whether to keep the TSV file if `file` is a ZIP archive or
  represents a URI. Is ignored if `file` is already a TSV. If `TRUE` the
  tsv file will be kept in the same directory as the ZIP archive.

- facts:

  List. A named list of "facts" to extract from supplementary "fact"
  files in a local or online FinBIF data archive. Names can include one
  or more of `"record"`, `"event"` or `"document"`. Elements of the list
  are character vectors of the "facts" to be extracted and then joined
  to the return value.

- type_convert_facts:

  Logical. Should facts be converted from character to numeric or
  integer data where applicable?

- drop_na:

  Logical. A vector indicating which columns to check for missing data.
  Values recycled to the number of columns. Defaults to all columns.

- drop_facts_na:

  Logical. Should missing or "all `NA`" facts be dropped? Any value
  other than a length one logical vector with the value of TRUE will be
  interpreted as FALSE. Argument is ignored if `drop_na` is TRUE for all
  variables explicitly or via recycling. To only drop some
  missing/`NA`-data facts use `drop_na` argument.

- locale:

  Character. One of the supported two-letter ISO 639-1 language codes.
  Current supported languages are English, Finnish and Swedish. For data
  where more than one language is available the language denoted by
  `locale` will be preferred while falling back to the other languages
  in the order indicated above.

- skip:

  Integer. The number of lines of the data file to skip before beginning
  to read data (not including the header).

## Value

A `data.frame`, or if `count_only = TRUE` an integer.

## Examples

``` r
if (FALSE) { # \dontrun{

# Get occurrence data
finbif_occurrence_load(49381)

} # }
```
