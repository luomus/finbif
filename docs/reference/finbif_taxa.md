# Search the FinBIF taxa

Search the FinBIF database for taxon.

## Usage

``` r
finbif_taxa(
  name,
  n = 1,
  type = c("exact", "partial", "likely"),
  cache = getOption("finbif_use_cache")
)

common_name(name, locale = getOption("finbif_locale"))

scientific_name(name)

taxon_id(name)
```

## Arguments

- name:

  Character. The name or ID of a taxon. Or, for functions other than
  `finbif_taxa` a `finbif_taxa` object.

- n:

  Integer. Maximum number of matches to return. For types "exact" and
  "likely" only one taxon will be returned.

- type:

  Character. Type of match to make. Must be one of `exact`, `partial` or
  `likely`.

- cache:

  Logical or Integer. If `TRUE` or a number greater than zero, then
  data-caching will be used. If not logical then cache will be
  invalidated after the number of hours indicated by the argument.

- locale:

  Character. One of the supported two-letter ISO 639-1 language codes.
  Current supported languages are English, Finnish and Swedish. For data
  where more than one language is available the language denoted by
  `locale` will be preferred while falling back to the other languages
  in the order indicated above.

## Value

For `finbif_taxa` a `finbif_taxa` object. Otherwise, a character vector.

## Examples

``` r
if (FALSE) { # \dontrun{

# Search for a taxon
finbif_taxa("Ursus arctos")

# Use partial matching
finbif_taxa("Ursus", n = 10, "partial")

# Get Swedish name of Eurasian Eagle-owl
common_name("Bubo bubo", "sv")

# Get scientific name of "Otter"
scientific_name("Otter")

# Get taxon identifier of "Otter"
taxon_id("Otter")

} # }
```
