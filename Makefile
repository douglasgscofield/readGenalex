# A general Makefile for R packages.
#
# In its current form it assumes you have only R code in your package, use
# Roxygen2 for documentation, knitr and markdown for vignettes, are using
# the inst/ directory to contain e.g., a CITATION file, and have a NEWS.md file
# that you want converted to plaintext for package release.  As I get more
# sophisticated with package contents, its utility will expand.
#
# Tools required:      R (via the command line), pandoc (same)
# R packages required: devtools, rmarkdown, knitr
#
# As for the package environment, it requires a preliminary version to be set
# up, with a DESCRIPTION file in place containing Package: and Version: lines.
# It uses these to determine the package name and version.
#
# Targets
# -------
#
# 'make all' or simply 'make' builds package pieces but not the package
# tarball.  It echoes package variables, builds the NEWS file, documentation
# (man/*.Rd files), and vignettes.
#
# 'make build' creates a package tarball in the parent directory, '..' by 
# default (set in PARENTDIR).
#
# 'make check' and 'make check-cran' create a directory in the package
# directory called CHECKDIR ('check_tmp' by default) and unpack the tarball
# inside it when checking.
#
# 'make clean' removes CHECKDIR and the package tarball
#
# Other targets:
#
# vars:       Echoes various variables
# doc:        Builds roxygen2 documentation using devtools::document()
# vignettes:  Builds vignettes using devtools::build_vignettes(), 
#             working around an apparent bug where R can't find pandoc
# NEWS:       Builds plaintext NEWS from NEWS.md using pandoc
#
# build, check and check-cran use a few other utility targets.
#
# Thanks to Karl Broman (http://kbroman.org/pkg_primer/pages/docs.html)
# for the should-have-been-obvious idea to include a Makefile here.  I
# too still do a lot from the command line.

PACKAGE = $(shell grep '^Package: ' DESCRIPTION | awk '{ print $$2 }')
VERSION = $(shell grep '^Version: ' DESCRIPTION | awk '{ print $$2 }')
PARENTDIR = ..
TARBALL = $(PACKAGE)_$(VERSION).tar.gz
TARBALL_LOC = $(PARENTDIR)/$(TARBALL)
CHECKDIR = check_tmp
PANDOC = pandoc

all: vars NEWS data doc vignettes

vars:
	@echo PACKAGE = "$(PACKAGE)"
	@echo VERSION = "$(VERSION)"
	@echo PARENTDIR = "$(PARENTDIR)"
	@echo TARBALL = "$(TARBALL)"
	@echo TARBALL_LOC = "$(TARBALL_LOC)"
	@echo CHECKDIR = "$(CHECKDIR)"
	@echo PANDOC = "$(PANDOC)"
	@echo

NEWS: NEWS.md
	$(PANDOC) -f markdown -t plain -o $@ $^

# can this data section be automated a bit?
data: data/Qagr_adult_genotypes.RData data/Qagr_pericarp_genotypes.RData

data/Qagr_adult_genotypes.RData: inst/extdata/Qagr_adult_genotypes.txt
	cd data && R --quiet -e 'source("../R/readGenalex.R"); Qagr_adult_genotypes <- readGenalex("../inst/extdata/Qagr_adult_genotypes.txt"); save(Qagr_adult_genotypes, file = "Qagr_adult_genotypes.RData")'

data/Qagr_pericarp_genotypes.RData: inst/extdata/Qagr_pericarp_genotypes.txt
	cd data && R --quiet -e 'source("../R/readGenalex.R"); Qagr_pericarp_genotypes <- readGenalex("../inst/extdata/Qagr_pericarp_genotypes.txt"); save(Qagr_pericarp_genotypes, file = "Qagr_pericarp_genotypes.RData")'

doc:
	R --quiet -e 'devtools::document()'

vignettes:
	if test -d vignettes ; then \
		RSTUDIO_PANDOC=`which pandoc` R --quiet -e 'devtools::build_vignettes()' ; \
	else \
		echo "No vignettes to build" ; \
	fi

$(CHECKDIR): $(TARBALL_LOC)
	rm -rf $(CHECKDIR) && mkdir $(CHECKDIR) && cp $(TARBALL_LOC) $(CHECKDIR)

$(TARBALL_LOC): NEWS doc vignettes R/*.R man/*.Rd data/* inst/*
	cd $(PARENTDIR) && RSTUDIO_PANDOC=`which pandoc` R CMD build $(PACKAGE)

build: $(TARBALL_LOC)

check: $(CHECKDIR)
	cd $(CHECKDIR) && R CMD check $(TARBALL)

check-cran: clean $(CHECKDIR)
	cd $(CHECKDIR) && R CMD check --as-cran $(TARBALL)

clean:
	rm -rf $(CHECKDIR)
	rm -f $(TARBALL_LOC)

.PHONY: vars doc build vignettes
