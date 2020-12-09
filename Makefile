PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
RSCRIPT = Rscript --no-init-file
export _R_CHECK_SYSTEM_CLOCK_ = false
export _R_CHECK_FUTURE_FILE_TIMESTAMPS_ = false

all: check_deps_only check clean

dev_deps:
	${RSCRIPT} -e "stopifnot(requireNamespace('data.tree', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('details', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('devtools', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('codemetar', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('ISOcodes', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('ows4R', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('pkgdown', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('sf', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('stats', quietly = TRUE))"
	${RSCRIPT} -e "stopifnot(requireNamespace('usethis', quietly = TRUE))"

build: doc
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check_deps_only: pkgdown codemeta.json
	cd ..;\
	_R_CHECK_DEPENDS_ONLY_=true R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

check: pkgdown codemeta.json
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

pkgdown: vignettes README.md
	${RSCRIPT} -e "pkgdown::build_site()"

README.md: README.Rmd NEWS.md
	${RSCRIPT} -e "knitr::knit('$<')"

NEWS.md: inst/NEWS.Rd install
	${RSCRIPT} -e "tools::Rd2HTML('$<', 'inst/NEWS.html')"
	sed -i 's/h3>/h1>/g' inst/NEWS.html
	pandoc -s inst/NEWS.html -o inst/NEWS.md -t gfm
	sed -i '1,6d' inst/NEWS.md
	sed -i 's/# ${PKGNAME} version/# ${PKGNAME}/g' inst/NEWS.md
	cp inst/NEWS.md NEWS.md
	$(RM) inst/NEWS.html inst/NEWS.md

vignettes: install
	cd inst/vign;\
	${RSCRIPT} -e "for (i in list.files('.', '.Rmd$$')) knitr::knit(i)";\
	sed -i 's/```details/```r/g' *.md;\
	cp *.md ../../vignettes;\
	cp ../man/figures/* ../../man/figures;\
	cd ../../vignettes;\
	for f in *.md; do mv -- "$$f" "$$(basename "$$f" .md).Rmd"; done;\
	cd ../;\
	${RSCRIPT} -e "devtools::build_vignettes()"

doc: R/sysdata.rda
	${RSCRIPT} -e "devtools::document()"

R/sysdata.rda: dev_deps $(wildcard data-raw/*.R)
	${RSCRIPT} data-raw/sysdata.R

codemeta.json: dev_deps DESCRIPTION
	${RSCRIPT} -e "codemetar::write_codemeta()"

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
