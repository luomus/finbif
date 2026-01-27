# finbif: Interface for the 'Finnish Biodiversity Information Facility' API

A programmatic interface to the 'Finnish Biodiversity Information
Facility' ('FinBIF') API (<https://api.laji.fi>). 'FinBIF' aggregates
Finnish biodiversity data from multiple sources in a single open access
portal for researchers, citizen scientists, industry and government.
'FinBIF' allows users of biodiversity information to find, access,
combine and visualise data on Finnish plants, animals and
microorganisms. The 'finbif' package makes the publicly available data
in 'FinBIF' easily accessible to programmers. Biodiversity information
is available on taxonomy and taxon occurrence. Occurrence data can be
filtered by taxon, time, location and other variables. The data accessed
are conveniently preformatted for subsequent analyses.

## Package options

- `finbif_api_url`:

  Character. The base url of the API to query. Default:
  `"https://api.laji.fi"`

- `finbif_api_version`:

  Character. The API version to use. Default: `"v0"`

- `finbif_allow_query`:

  Logical. Should remote API queries by allowed. Default: `TRUE`

- `finbif_use_cache`:

  Logical or Integer. If `TRUE` or a number greater than zero, then
  data-caching will be used. If not logical then cache will be
  invalidated after the number of hours indicated by the value. Default:
  `TRUE`

- `finbif_use_cache_metadata`:

  Logical or Integer. If `TRUE` or a number greater than zero, then
  metadata-caching will be used. If not logical then cache will be
  invalidated after the number of hours indicated by the value. Default:
  `TRUE`

- `finbif_cache_path`:

  Character. The path to the directory where to store cached API
  queries. If unset (the default) in memory caching is used.

- `finbif_tz`:

  Character. The timezone used by `finbif` functions that compute dates
  and times. Default:
  [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html)

- `finbif_locale`:

  Character. One of the supported two-letter ISO 639-1 language codes.
  Current supported languages are English, Finnish and Swedish. By
  default, the system settings are used to set this option if they are
  set to one of the supported languages, otherwise English is used.

- `finbif_hide_progress`:

  Logical. Global option to suppress progress indicators for
  downloading, importing and processing FinBIF records. Default: `FALSE`

## See also

Useful links:

- <https://github.com/luomus/finbif>

- <https://finbif-docs-dev.netlify.app/>

- Report bugs at <https://github.com/luomus/finbif/issues>

## Author

**Maintainer**: William K. Morris <willi@mmorris.email>
([ORCID](https://orcid.org/0000-0002-8686-4154))

Other contributors:

- Finnish Museum of Natural History - Luomus
  ([ROR](https://ror.org/03tcx6c30)) \[copyright holder\]

- Finnish Biodiversity Information Facility
  ([ROR](https://ror.org/01c0yey93)) \[copyright holder\]
