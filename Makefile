all: clean data docs test check install

clean:
	rm -rf man/*
	rm -rf data/*
	rm -rf docs/*
	rm -rf inst/doc/*
	rm -rf vignettes/*

docs: man readme site vigns

data:
	Rscript --slave inst/extdata/simulate_data.R

man:
	R --slave -e "devtools::document()"

readme:
	rm -rf inst/vign/readme-figure
	rm -rf inst/vign/gh-README.Rmd
	rm -rf inst/vign/gh-README.md
	cd inst/vign;\
	cp README.Rmd gh-README.Rmd;\
	sed -i 1,11d gh-README.Rmd;\
	sed -i -e '1i---\' gh-README.Rmd;\
	sed -i -e '1ioutput: github_document\' gh-README.Rmd;\
	sed -i -e '1ititle: prioritizr\' gh-README.Rmd;\
	sed -i -e '1i---\' gh-README.Rmd;\
	R --slave -e "knitr::knit('gh-README.Rmd')"
	mv -f inst/vign/gh-README.md ./README.md
	rm -rf inst/vign/gh-README.Rmd
	mv inst/vign/figure inst/vign/readme-figure
	sed -i 1,5d README.md
	sed -i 's|figure|inst/vign/readme-figure|g' README.md

vigns:
	mkdir -p vignettes
	cp -f inst/vign/*.Rmd vignettes/
	rm vignettes/placeholder.Rmd
	rm vignettes/README.Rmd
	R --slave -e "devtools::build_vignettes()"
	rm -rf vignettes/*
	mkdir -p vignettes
	cp inst/vign/placeholder.Rmd vignettes/prioritizr.Rmd
	touch inst/doc/*

site:
	mkdir -p vignettes
	cp -f inst/vign/*.Rmd vignettes
	rm vignettes/placeholder.Rmd
	rm vignettes/README.Rmd
	R --slave -e "pkgdown::build_home()"
	R --slave -e "pkgdown::build_reference()"
	R --slave -e "pkgdown::build_articles()"
	R --slave -e "pkgdown::build_news()"
	cp -R inst/vign/readme-figure docs/
	sed -i 's|img src="inst/vign/readme-figure|img src="readme-figure|g' docs/index.html
	rm -rf vignettes/*
	rm -rf inst/doc/*

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check()" >> check.log 2>&1
	echo "\n===== SPELL CHECK =====\n" >> check.log 2>&1
	R --slave -e "devtools::spell_check(ignore = as.character(read.table('inst/extdata/dictionary.txt')[[1]]))" >> check.log 2>&1
	echo "\n===== GOOD PRACTICE CHECK =====\n" >> check.log 2>&1
	R --slave -e "goodpractice::gp()" >> check.log 2>&1

checkwb:
	R --slave -e "devtools::build_win()"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "devtools::install_local('../conserve')"

.PHONY: clean data docs readme site test check checkwb build  install man
