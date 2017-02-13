all: clean data docs readme vigns site test check install

clean:
	rm -rf data/*
	rm -rf docs/*
	rm -rf inst/doc/*
	rm -rf vignettes/*

data:
	Rscript --slave inst/extdata/simulate_data.R
docs:
	R --slave -e "devtools::document()"

readme:
	rm -rf inst/vign/readme-figure
	rm -rf inst/vign/gh-README.Rmd
	rm -rf inst/vign/gh-README.md
	cd inst/vign;\
	cp README.Rmd gh-README.Rmd;\
	sed -i 1,14d gh-README.Rmd;\
	sed -i -e '1i---\' gh-README.Rmd;\
	sed -i -e '1ioutput: github_document\' gh-README.Rmd;\
	sed -i -e '1ititle: prioritizr\' gh-README.Rmd;\
	sed -i -e '1i---\' gh-README.Rmd;\
	R --slave -e "knitr::knit('gh-README.Rmd')"
	mv -f inst/vign/gh-README.md ./README.md
	mv -f inst/vign/figure inst/vign/readme-figure
	rm -rf inst/vign/gh-README.Rmd
	sed -i 1,5d README.md
	sed -i 's|figure|inst/vign/readme-figure|g' README.md

vigns:
	mkdir -p vignettes
	cp -f inst/vign/*.Rmd vignettes/
	R --slave -e "devtools::load_all();devtools::build_vignettes()"
	rm -rf vignettes/*
	mkdir -p vignettes
	cp -f inst/vign/placeholder.Rmd vignettes/syntax.Rmd
	cp -f inst/vign/placeholder.Rmd vignettes/build.Rmd
	cp -f inst/vign/placeholder.Rmd vignettes/modify.Rmd
	cp -f inst/vign/placeholder.Rmd vignettes/README.Rmd
	touch inst/doc/*

site:
	mkdir -p vignettes
	cp -f inst/vign/*.Rmd vignettes
	R --slave -e "devtools::load_all();pkgdown::build_site()"
	rm -rf vignettes/*
	rm -rf inst/doc/*

test:
	R --slave -e "devtools::test()"

check:
	R --slave -e "devtools::check()"
	R --slave -e "devtools::build_win()"

checkwb:
	R --slave -e "devtools::check()"
	R --slave -e "devtools::build_win()"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "devtools::install_local('../conserve')"

.PHONY: clean data docs readme site test check checkwb build  install 
