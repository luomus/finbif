PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
RSCRIPT = Rscript --no-init-file

all: check clean

build: doc
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: vignettes README.md codemeta
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

README.md: README.Rmd install
	${RSCRIPT} -e "knitr::knit('$<')"

vignettes: install
	cd inst/vign;\
	${RSCRIPT} -e "for (i in list.files('.', '.Rmd$$')) knitr::knit(i)";\
	cp *.md ../../vignettes;\
	cd ../../vignettes;\
	for f in *.md; do mv -- "$$f" "$$(basename "$$f" .md).Rmd"; done;\
	cd ../;\
	${RSCRIPT} -e "devtools::build_vignettes()"

doc: sysdata
	${RSCRIPT} -e "devtools::document()"

sysdata:
	${RSCRIPT} data-raw/sysdata.R

codemeta:
	${RSCRIPT} -e "codemetar::write_codemeta()"

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
