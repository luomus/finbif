
# finbif <img src="man/figures/logo.png" align="right" alt="" width="120">
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing) [![CRAN status](https://www.r-pkg.org/badges/version-last-release/finbif)](https://cran.r-project.org/package=finbif) [![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/finbif?color=brightgreen)](https://cran.r-project.org/package=finbif)

Branch |Travis |AppVeyor |Codecov
------ |------ |-------- |-------
master |[![Build Status](https://travis-ci.com/luomus/finbif.svg?branch=master)](https://travis-ci.com/luomus/finbif/branches) |[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/luomus/finbif?branch=master&svg=true)](https://ci.appveyor.com/project/luomus/finbif/branch/master) |[![codecov](https://codecov.io/gh/luomus/finbif/branch/master/graph/badge.svg)](https://codecov.io/github/luomus/finbif/branch/master)
dev |[![Build Status](https://travis-ci.com/luomus/finbif.svg?branch=dev)](https://travis-ci.com/luomus/finbif/branches) |[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/luomus/finbif?branch=dev&svg=true)](https://ci.appveyor.com/project/luomus/finbif/branch/dev) |[![codecov](https://codecov.io/gh/luomus/finbif/branch/dev/graph/badge.svg)](https://codecov.io/github/luomus/finbif/branch/dev)

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

```r
install.packages("finbif")
```

You can also install the latest development version of `finbif` from
[GitHub](https://github.com),

```r
remotes::install_github("luomus/finbif@dev")
```

## Documentation
Read the online documentation for the current stable version of
finbif [here](https://luomus.github.io/finbif), or the latest development
version of finbif [here](https://finbif-docs-dev.netlify.com).

## Getting a FinBIF access token
First load the `finbif` R package.

```r
library(finbif)
```

To use the FinBIF API you must first request and set a personal access token.
You can request an API token to be sent to your email address with the function
`finbif_get_token`.

```r
finbif_request_token("your@email.com")
```

Copy the access token that was sent to your email and set it as the environment
variable `FINBIF_ACCESS_TOKEN` either for the current session,

```r
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

```r
finbif_occurrence("Cygnus cygnus", n = 100)
```

----

## Contributing
Development is a community effort, and we encourage participation. Please read
[the contribution guide](https://github.com/luomus/finbif/blob/master/CONTRIBUTING.md)
for details.

Please note that the 'finbif' project is released with a
[Contributor Code of Conduct](https://github.com/luomus/finbif/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
