PACKAGE = readGenalex
VERSION = $(shell grep Version DESCRIPTION | awk '{ print $$2 }')
PARENTDIR = ..
TARBALL = $(PACKAGE)_$(VERSION).tar.gz
TARBALL_LOC = $(PARENTDIR)/$(TARBALL)
CHECKDIR = check_tmp

all: vars doc

vars:
	echo PACKAGE = "$(PACKAGE)"
	echo VERSION = "$(VERSION)"
	echo PARENTDIR = "$(PARENTDIR)"
	echo TARBALL = "$(TARBALL)"
	echo TARBALL_LOC = "$(TARBALL_LOC)"
	echo CHECKDIR = "$(CHECKDIR)"

doc:
	#echo "Roxygen2 not yet in use here"
	R --quiet -e 'devtools::document()'

build: $(TARBALL_LOC)

# not currently inst/*
$(TARBALL_LOC): doc R/*.R man/*.Rd data/*
	cd $(PARENTDIR) && R CMD build $(PACKAGE)

$(CHECKDIR): $(TARBALL_LOC)
	rm -rf $(CHECKDIR) && mkdir $(CHECKDIR) && cp $(TARBALL_LOC) $(CHECKDIR)

check: $(CHECKDIR)
	cd $(CHECKDIR) && R CMD check $(TARBALL)

check-cran: clean $(CHECKDIR)
	cd $(CHECKDIR) && R CMD check --as-cran $(TARBALL)

clean:
	rm -rf $(CHECKDIR)
	rm -f $(TARBALL_LOC)

.PHONY: doc build
