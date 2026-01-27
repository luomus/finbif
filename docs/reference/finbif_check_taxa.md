# Check FinBIF taxa

Check that taxa are in the FinBIF database.

## Usage

``` r
finbif_check_taxa(taxa, cache = getOption("finbif_use_cache"))
```

## Arguments

- taxa:

  Character (or list of named character) vector(s). If a list each
  vector can have the name of a taxonomic rank (genus, species, etc.,).
  The elements of the vectors should be the taxa to check.

- cache:

  Logical or Integer. If `TRUE` or a number greater than zero, then
  data-caching will be used. If not logical then cache will be
  invalidated after the number of hours indicated by the argument.

## Value

An object of class `finbif_taxa`. A list with the same form as `taxa`.

## Examples

``` r
if (FALSE) { # \dontrun{

# Check a scientific name
finbif_check_taxa("Cygnus cygnus")

# Check a common name
finbif_check_taxa("Whooper swan")

# Check a genus
finbif_check_taxa("Cygnus")

# Check a list of taxa
finbif_check_taxa(
  list(
    species = c("Cygnus cygnus", "Ursus arctos"),
    genus   = "Betula"
  )
)
} # }
```
