
# finbif <img src="man/figures/logo.png" align="right" alt="" width="120">

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CircleCI](https://circleci.com/bb/luomus/finbif.svg?style=shield&circle-token=0c7c2580ef1ca3246d3da8ba60064aeaf9c8eecd)](https://circleci.com/bb/luomus/finbif)
[![codecov](https://codecov.io/bb/luomus/finbif/branch/master/graph/badge.svg?token=erk5D6H4i2)](https://codecov.io/bb/luomus/finbif)
[![Netlify
Status](https://api.netlify.com/api/v1/badges/5fdd166e-0155-4992-9009-82434fefe4f4/deploy-status)](https://app.netlify.com/sites/finbif/deploys)

An R interface to the FinBIF (Finnish Biodiversity Information Facility)
API (api.laji.fi).

## Installation

You can install the development version of finbif from
[Bitbucket](https://bitbucket.org) with:

``` r
install.packages("remotes")
remotes::install_bitbucket("luomus/finbif")
```

or from [GitHub](https://github.com),

``` r
remotes::install_github("wkmor1/finbif")
```

## Documentation

Read the online documentation [here](https://finbif.netlify.com).

## Getting a FinBIF access token

First load the finbif R package.

``` r
library(finbif)
```

To use the FinBIF API you must first request and set a personal access
token. You can request an API token to be sent to your email address
with the function `finbif_get_token`.

``` r
finbif_request_token("your@email.com")
```

Copy the access token that was sent to your email and set it as the
environment variable `FINBIF_ACCESS_TOKEN` either for the current
session,

``` r
Sys.setenv(
  FINBIF_ACCESS_TOKEN = "xtmSOIxjPwq0pOMB1WvcZgFLU9QBklauOlonWl8K5oaLIx8RniJLrvcJU4v9H7Et"
)
# Note: the above is not a real access token. Do not try using it.
```

, or by adding it to a `Renviron` startup file (see
[here](https://rviews.rstudio.com/2017/04/19/r-for-enterprise-understanding-r-s-startup/)
for details).

## Usage

Download occurrence data from finbif.

``` r
finbif_occurrence("Cygnus cygnus", n = 100)
#> Downloading page 1
#> Records downloaded: 100
#> Records available: 54592
#> A data.frame [100 x 27]
#>    scientific_name abundance lat_wgs84 lon_wgs84 date_start
#> 1    Cygnus cygnus         1  60.56745  21.57191 2019-06-30
#> 2    Cygnus cygnus         1  61.32290  28.56811 2019-05-09
#> 3    Cygnus cygnus         1  61.32294  28.56868 2019-05-10
#> 4    Cygnus cygnus         1  61.07692  21.49222 2019-05-16
#> 5    Cygnus cygnus         1  62.25243  25.70933 2019-05-16
#> 6    Cygnus cygnus         1  60.83907  21.25772 2019-05-16
#> 7    Cygnus cygnus         1  61.12486  21.54164 2019-05-21
#> 8    Cygnus cygnus         1  61.63413  22.90005 2019-05-19
#> 9    Cygnus cygnus         1  60.46200  26.94900 2019-05-11
#> 10   Cygnus cygnus         1  61.99900  22.16100 2019-05-11
#> ...with 90 more records and 22 more fields:
#> taxon_rank, country, province, municipality, wkt_wgs84,
#> line_length_m, area_m2, date_end, hour_start, hour_end,
#> minutes_start, minutes_end, record_id, event_id,
#> collection_id, is_breeding_site, any_issues,
#> record_reliable, taxon_reliability, sex,
#> document_reliablity, coordinate_accuracy
```

-----

## Contributing

Development is a community effort, and we encourage participation.
Please read [the contribution guide](CONTRIBUTING.md) for details.

Please note that the ‘finbif’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
