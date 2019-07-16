PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
RSCRIPT = Rscript --no-init-file

all: check clean

install: build
	R CMD INSTALL . && rm *.tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

build: codemeta
	cd ..;\
	R CMD build $(PKGSRC)

codemeta: pkgdown
	${RSCRIPT} -e "codemetar::write_codemeta()"

pkgdown: readme doc
	${RSCRIPT} -e "pkgdown::build_site()"

readme:
	${RSCRIPT} -e "rmarkdown::render('README.Rmd')" && rm README.html

doc: sysdata
	${RSCRIPT} -e "devtools::document()"

sysdata:
	${RSCRIPT} data-raw/sysdata.R

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
