
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

## Documentation

Read the online documentation [here](https://finbif.netlify.com).

## Getting a FinBIF access token

First load the finbif R package.

``` r
library(finbif)
```

To use the FinBIF API you must first request and set an personal access
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
#> Records available: 54581
#> A data.frame [100 x 27]
#>    scientific_name taxon_rank abundance country            province
#> 1    Cygnus cygnus    species         1   Suomi Varsinais-Suomi (V)
#> 2    Cygnus cygnus    species         1   Suomi     Etelä-Savo (ES)
#> 3    Cygnus cygnus    species         1   Suomi     Etelä-Savo (ES)
#> 4    Cygnus cygnus    species         1   Suomi      Satakunta (St)
#> 5    Cygnus cygnus    species         1   Suomi   Pohjois-Häme (PH)
#> 6    Cygnus cygnus    species         1   Suomi Varsinais-Suomi (V)
#> 7    Cygnus cygnus    species         1   Suomi      Satakunta (St)
#> 8    Cygnus cygnus    species         1   Suomi      Satakunta (St)
#> 9    Cygnus cygnus    species         1   Suomi  Etelä-Karjala (EK)
#> 10   Cygnus cygnus    species         1   Suomi      Satakunta (St)
#>    municipality lat_wgs84 lon_wgs84
#> 1    Taivassalo  60.56745  21.57191
#> 2    Ruokolahti  61.32290  28.56811
#> 3    Ruokolahti  61.32294  28.56868
#> 4         Rauma  61.07692  21.49222
#> 5     Jyväskylä  62.25243  25.70933
#> 6  Uusikaupunki  60.83907  21.25772
#> 7         Rauma  61.12486  21.54164
#> 8    Hämeenkyrö  61.63413  22.90005
#> 9         Kotka  60.46200  26.94900
#> 10    Honkajoki  61.99900  22.16100
#> ...with 90 more records and 19 more fields:
#> wkt_wgs84, line_length_m, area_m2, date_start, date_end, hour_start, hour_end,
#> minutes_start, minutes_end, record_id, event_id, collection_id, is_breeding_site, any_issues,
#> record_reliable, taxon_reliability, sex, document_reliablity, coordinate_accuracy,
```
