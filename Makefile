all: clean initc data docs test check

clean:
	rm -rf man/*
	rm -rf data/*
	rm -rf docs/*
	rm -rf inst/doc/*

initc:
	R --slave -e "tools::package_native_routine_registration_skeleton('.', 'src/init.c', character_only = FALSE)"

docs: man readme site vigns

data:
	Rscript --slave inst/extdata/simulate_data.R

man:
	R --slave -e "devtools::document()"

readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

vigns:
	R --slave -e "devtools::build_vignettes()"

site:
	R --slave -e "pkgdown::build_site()"

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check()" >> check.log 2>&1
	# echo "\n===== SPELL CHECK =====\n" >> check.log 2>&1
	# R --slave -e "devtools::spell_check(ignore = as.character(read.table('inst/extdata/dictionary.txt')[[1]]))" >> check.log 2>&1
	# echo "\n===== GOOD PRACTICE CHECK =====\n" >> check.log 2>&1
	# R --slave -e "goodpractice::gp()" >> check.log 2>&1

checkwb:
	R --slave -e "devtools::build_win()"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "devtools::install_local('../prioritizr')"

.PHONY: clean data docs readme site test check checkwb build  install man
