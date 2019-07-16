PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
RSCRIPT = Rscript --no-init-file

all: check clean

check: codemeta
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

codemeta: pkgdown
	${RSCRIPT} -e "codemetar::write_codemeta()"

pkgdown: readme
	${RSCRIPT} -e "pkgdown::build_site()"

readme: vignettes
	${RSCRIPT} -e "rmarkdown::render('README.Rmd')" && rm README.html

vignettes: install
  ${RSCRIPT} -e "devtools::build_vignettes()"

install: build
	R CMD INSTALL .

build: doc
	cd ..;\
	R CMD build $(PKGSRC)

doc: sysdata
	${RSCRIPT} -e "devtools::document()"

sysdata:
	${RSCRIPT} data-raw/sysdata.R

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
