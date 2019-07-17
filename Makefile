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

check: pkgdown codemeta
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

readme: install
	${RSCRIPT} -e "rmarkdown::render('README.Rmd')" && rm README.html

pkgdown: vignettes readme
	${RSCRIPT} -e "pkgdown::build_site()"

vignettes: install
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
