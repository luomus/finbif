# Contributing

### Fixing typos

Small typos or grammatical errors in documentation may be edited so long as the
changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal
[reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

*  We recommend that you create a Git branch for each pull request (PR).
*  Look at the build status before and after making changes.
The `README` contains badges for any continuous integration services used
by the package.
*  New code should follow the style used in the project.
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html),
for documentation.
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept.
*  For user-facing changes, add a section to the top of `inst/NEWS.Rd` below the
current development version header describing the changes made followed by your
username, and links to relevant issue(s)/PR(s).

### Code of Conduct

Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
