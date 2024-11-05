
# FinBIF R package <img src="man/figures/logo.png" align="right" alt="" width="120">

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License](https://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat)](https://opensource.org/license/mit-0)
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/finbif)](https://cran.r-project.org/package=finbif)
[![r-universe](https://luomus.r-universe.dev/badges/finbif)](https://luomus.r-universe.dev)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/finbif?color=brightgreen)](https://cran.r-project.org/package=finbif)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3612814.svg)](https://doi.org/10.5281/zenodo.3612814)
[![Build Status](https://github.com/luomus/finbif/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/luomus/finbif/actions)
[![codecov](https://codecov.io/gh/luomus/finbif/branch/main/graph/badge.svg)](https://app.codecov.io/github/luomus/finbif/branch/main)
<!-- badges: end -->

The `finbif` R package is a programmatic interface to the
[Finnish Biodiversity Information Facility (FinBIF) API](https://api.laji.fi).
FinBIF aggregates Finnish biodiversity data from multiple sources in a single
open access portal for researchers, citizen scientists, industry and government.
FinBIF allows users of biodiversity information to find, access, combine and
visualise data on Finnish plants, animals and microorganisms. The `finbif`
R package makes the publicly available data in FinBIF easily accessible to
programmers. Biodiversity information is available on taxonomy and taxon
occurrence. Occurrence data can be filtered by taxon, time, location and other
variables. The data accessed are conveniently preformatted for subsequent
analyses.

## Installation
You can install the current stable version of `finbif` from
[CRAN](https://cran.r-project.org),

``` r
install.packages("finbif")
```

You can also install the latest development version of `finbif` from
[GitHub](https://github.com),

``` r
remotes::install_github("luomus/finbif@dev")
```

## Documentation
Read the online documentation for the current stable version of
`finbif` [here](https://luomus.github.io/finbif/), or the latest development
version of `finbif` [here](https://finbif-docs-dev.netlify.app).

## Getting a FinBIF access token
First load the `finbif` R package.

``` r
library(finbif)
```

To use the FinBIF API you must first request and set a personal access token.
You can request an API token to be sent to your email address with the function
`finbif_get_token`.

``` r
finbif_request_token("your@email.com")
```

Copy the access token that was sent to your email and set it as the environment
variable `FINBIF_ACCESS_TOKEN` either for the current session,

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
Download occurrence data from FinBIF.

``` r
finbif_occurrence("Cygnus cygnus", n = 100)
```

## Citation
Data from FinBIF comes from many sources. For information on citing
FinBIF itself and the contributed datasets please visit this link:

<https://laji.fi/en/about/2986>

To cite the finbif R package in publications please use:

  Morris, William K. (2024). Introduction to the finbif package. R
  package version 0.9.9, https://doi.org/10.5281/zenodo.3612814

----

## Contributing
Development is a community effort, and we encourage participation. Please read
[the contribution guide](https://github.com/luomus/finbif/blob/main/CONTRIBUTING.md)
for details.

Please note that the 'finbif' project is released with a
[Contributor Code of Conduct](https://github.com/luomus/finbif/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
