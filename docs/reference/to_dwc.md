# Convert variable names

Convert variable names to Darwin Core or FinBIF R package native style.

## Usage

``` r
to_dwc(...)

to_native(...)

from_schema(..., to = c("native", "dwc"), file = c("none", "citable", "lite"))
```

## Arguments

- ...:

  Character. Variable names to convert. For `to_dwc` and `to_native` the
  names must be in the opposite format. For `from_schema` the names must
  be from the FinBIF schema (e.g., names returned by
  https://api.laji.fi) or a FinBIF download file (citable or lite).

- to:

  Character. Type of variable names to convert to.

- file:

  Character. For variable names that are derived from a FinBIF download
  file which type of file.

## Value

Character vector.

## Examples

``` r
to_dwc("record_id", "date_time", "scientific_name")
#> [1] "occurrenceID"   "eventDateTime"  "scientificName"
```
