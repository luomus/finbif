
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
c_cygnus <- finbif_occurrence(list(species = "Cygnus cygnus"), n = 100)
#> Downloading page 1
str(c_cygnus)
#> 'data.frame':    100 obs. of  27 variables:
#>  $ scientific_name    : chr  "Cygnus cygnus" "Cygnus cygnus" "Cygnus cygnus" "Cygnus cygnus" ...
#>  $ taxon_rank         : chr  "http://tun.fi/MX.species" "http://tun.fi/MX.species" "http://tun.fi/MX.species" "http://tun.fi/MX.species" ...
#>  $ abundance          : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ country            : chr  "Suomi" "Suomi" "Suomi" "Suomi" ...
#>  $ province           : chr  "Varsinais-Suomi (V)" "Etelä-Savo (ES)" "Etelä-Savo (ES)" "Satakunta (St)" ...
#>  $ municipality       : chr  "Taivassalo" "Ruokolahti" "Ruokolahti" "Rauma" ...
#>  $ lat_wgs84          : num  60.6 61.3 61.3 61.1 62.3 ...
#>  $ lon_wgs84          : num  21.6 28.6 28.6 21.5 25.7 ...
#>  $ wkt_wgs84          : chr  "POINT(21.571912 60.567453)" "POINT(28.568112 61.322899)" "POINT(28.568678 61.322937)" "POINT(21.492219 61.076921)" ...
#>  $ line_length_m      : int  NA NA NA NA NA 2359 2862 NA NA NA ...
#>  $ area_m2            : int  1 1 1 1 174956 467840 845914 3034 1 1 ...
#>  $ date_start         : chr  "2019-06-30" "2019-05-09" "2019-05-10" "2019-05-16" ...
#>  $ date_end           : chr  "2019-06-30" "2019-05-09" "2019-05-10" "2019-05-16" ...
#>  $ hour_start         : int  NA 11 7 11 NA 23 9 NA NA NA ...
#>  $ hour_end           : int  NA NA 12 12 NA 0 11 NA NA NA ...
#>  $ minutes_start      : int  NA 30 45 25 NA 0 30 NA NA NA ...
#>  $ minutes_end        : int  NA NA 15 25 NA 0 15 NA NA NA ...
#>  $ record_id          : chr  "http://tun.fi/JX.1012218#13" "http://tun.fi/JX.997014#43" "http://tun.fi/JX.997301#55" "http://tun.fi/JX.997841#13" ...
#>  $ event_id           : chr  "http://tun.fi/JX.1012218#2" "http://tun.fi/JX.997014#2" "http://tun.fi/JX.997301#2" "http://tun.fi/JX.997841#2" ...
#>  $ collection_id      : chr  "http://tun.fi/HR.1747" "http://tun.fi/HR.1747" "http://tun.fi/HR.1747" "http://tun.fi/HR.1747" ...
#>  $ is_breeding_site   : logi  NA NA NA NA NA NA ...
#>  $ any_issues         : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ record_reliable    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ taxon_reliability  : chr  "NEUTRAL" "NEUTRAL" "NEUTRAL" "NEUTRAL" ...
#>  $ sex                : chr  NA NA NA NA ...
#>  $ document_reliablity: int  2 2 2 2 2 2 2 2 1 1 ...
#>  $ coordinate_accuracy: int  1 1 1 1 1000 5000 5000 1000 10000 10000 ...
```
