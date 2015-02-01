#TODO: come up with build for data
PACKAGE = readGenalex
VERSION = $(shell grep Version DESCRIPTION | awk '{ print $$2 }')
PARENTDIR = ..
TARBALL = $(PACKAGE)_$(VERSION).tar.gz
TARBALL_LOC = $(PARENTDIR)/$(TARBALL)
CHECKDIR = check_tmp
PANDOC = pandoc

#all: vars NEWS doc vignettes
all: vars NEWS doc

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

doc:
	R --quiet -e 'devtools::document()'

#vignettes:
#	RSTUDIO_PANDOC=`which pandoc` R --quiet -e 'devtools::build_vignettes()'

$(CHECKDIR): $(TARBALL_LOC)
	rm -rf $(CHECKDIR) && mkdir $(CHECKDIR) && cp $(TARBALL_LOC) $(CHECKDIR)

# not currently inst/*
$(TARBALL_LOC): NEWS doc vignettes R/*.R man/*.Rd data/*
	cd $(PARENTDIR) && R CMD build $(PACKAGE)

build: $(TARBALL_LOC)

check: $(CHECKDIR)
	cd $(CHECKDIR) && R CMD check $(TARBALL)

check-cran: clean $(CHECKDIR)
	cd $(CHECKDIR) && R CMD check --as-cran $(TARBALL)

clean:
	rm -rf $(CHECKDIR)
	rm -f $(TARBALL_LOC)

.PHONY: vars doc build
#.PHONY: vars doc build vignettes
