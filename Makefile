SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make >=4.0)
endif
.RECIPEPREFIX = >

PKGNM := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
RSCRIPT = Rscript --no-init-file
export _R_CHECK_SYSTEM_CLOCK_ = false
export _R_CHECK_FUTURE_FILE_TIMESTAMPS_ = false

all: dev_deps R/sysdata.rda sentinels/check codemeta.json clean
.PHONY: all

dev_deps:
> ${RSCRIPT} -e "stopifnot(requireNamespace('codemetar', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('data.table', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('data.tree', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('details', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('devtools', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('future', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('grDevices', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('jsonlite', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('ISOcodes', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('knitr', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('ows4R', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('pkgdown', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('readODS', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('readxl', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('rmarkdown', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('sf', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('testthat', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('usethis', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('vcr', quietly = TRUE))";\
> ${RSCRIPT} -e "stopifnot(requireNamespace('webfakes', quietly = TRUE))";
.PHONY: dev_deps

sentinels/check: sentinels/pkgdown $(shell find tests -type f)
> cd ..;\
> R CMD build $(PKGSRC);\
> R CMD INSTALL $(PKGNM)_$(PKGVERS).tar.gz;\
> _R_CHECK_DEPENDS_ONLY_=true R CMD check $(PKGNM)_$(PKGVERS).tar.gz --as-cran;\
> R CMD check $(PKGNM)_$(PKGVERS).tar.gz --as-cran;\
> cd $(PKGSRC);\
> mkdir -p $(@D);\
> touch $@

sentinels/pkgdown: sentinels/vignettes README.md NEWS.md sentinels/doc _pkgdown.yml
> echo "options(rmarkdown.html_vignette.check_title = FALSE)" > .Rprofile;\
> ${RSCRIPT} -e "pkgdown::build_site()";\
> rm .Rprofile;\
> mkdir -p $(@D);\
> touch $@

README.md: README.Rmd
> ${RSCRIPT} -e "knitr::knit('$<')"

NEWS.md: inst/NEWS.Rd
> ${RSCRIPT} -e "tools::Rd2HTML('$<', 'inst/NEWS.html')";\
> sed -i 's/h3>/h1>/g' inst/NEWS.html;\
> pandoc -s inst/NEWS.html -o inst/NEWS.md -t gfm;\
> sed -i '1,8d' inst/NEWS.md;\
> head -n -2 inst/NEWS.md > NEWS.md;\
> sed -i 's/# ${PKGNM} version/# ${PKGNM}/g' NEWS.md;\
> $(RM) inst/NEWS.html inst/NEWS.md

sentinels/vignettes: $(shell find inst/vign -type f)
> cd ..;\
> R CMD build $(PKGSRC);\
> R CMD INSTALL $(PKGNM)_$(PKGVERS).tar.gz;\
> cd $(PKGSRC)/inst/vign;\
> ${RSCRIPT} -e "for (i in list.files('.', '.Rmd$$')) knitr::knit(i)";\
> sed -i 's/```details/```r/g' *.md;\
> cp *.md ../../vignettes;\
> cp ../man/figures/* ../../man/figures;\
> cd ../../vignettes;\
> for f in *.md; do mv -- "$$f" "$$(basename "$$f" .md).Rmd"; done;\
> cd ..;\
> echo "options(rmarkdown.html_vignette.check_title = FALSE)" > .Rprofile;\
> ${RSCRIPT} -e "devtools::build_vignettes()";\
> rm .Rprofile;\
> mkdir -p $(@D);\
> touch $@

sentinels/doc: $(shell find R -type f) DESCRIPTION
> ${RSCRIPT} -e "devtools::document()";\
> mkdir -p $(@D);\
> touch $@

R/sysdata.rda: $(shell find data-raw -type f)
> ${RSCRIPT} data-raw/sysdata.R

codemeta.json: DESCRIPTION
> ${RSCRIPT} -e "codemetar::write_codemeta()"

clean:
> cd ..;\
> $(RM) -r $(PKGNM).Rcheck $(PKGNM)_$(PKGVERS).tar.gz
.PHONY: clean
