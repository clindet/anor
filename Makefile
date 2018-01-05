PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: doc build

doc:
	Rscript -e "devtools::document()"

build:
	cd ..;\
	R CMD build $(PKGSRC)

build2:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC)

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

check_cran: build
	cd ..;\
	R CMD check --as-cran $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/;

clean_test2:
	cd ./tests/testthat/ && $(RM) test_annotation.R test_big_file.R test_download.R test_parllel.R;

cleanall:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/;\
	$(RM) -ri $(DOWNLOAD_DIR);\
	$(RM) -ri $(DEST_DIR)

test:
	cd .;\
    Rscript -e "devtools::test()"

test2:
	cd ./tests/testthat/;\
	cp ./tests/testthat/slow/*.R ./tests/testthat/;\
	cd ./;\
    Rscript -e "devtools::test()"

format:
	cd .;\
	Rscript -e "library(formatR);options('formatR.indent'=2);tidy_dir('./R');tidy_dir('./tests/testthat');"

reduce_test:
	mkdir ./tests/testthat/bak;\
	mv ./tests/testthat/*.R ./tests/testthat/bak/;\
	mv ./tests/testthat/bak/$(name).R ./tests/testthat/

clean_reduce:
	cd ./tests/testthat;\
	mv bak/* ./;\
	rmdir bak;
