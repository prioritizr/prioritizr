all: clean initc data docs test check

clean:
	rm -rf man/*
	rm -rf data/*
	rm -rf docs/*
	rm -rf inst/doc/*

initc:
	R --slave -e "Rcpp::compileAttributes()"
	R --slave -e "tools::package_native_routine_registration_skeleton('.', 'src/init.c', character_only = FALSE)"

docs: man readme vigns site

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

spellcheck:
	echo "\n===== SPELL CHECK =====\n" > spell.log 2>&1
	R --slave -e "devtools::spell_check(ignore = readLines('inst/extdata/dictionary.txt'))" >> spell.log 2>&1

wbcheck:
	R --slave -e "devtools::build_win()"

gpcheck:
	echo "\n===== GOOD PRACTICES CHECK =====\n" > gp.log 2>&1
	R --slave -e "goodpractice::gp(checks = setdiff(goodpractice::all_checks(), c('covr', 'cyclocomp', 'no_description_depends', 'no_import_package_as_a_whole')), quiet = FALSE)" >> gp.log 2>&1

solarischeck:
	R --slave -e "rhub::check(platform = 'solaris-x86-patched', email = 'jeffrey.hanson@uqconnect.edu.au', show_status = FALSE)"

build:
	R --slave -e "devtools::build()"

install:
	R --slave -e "devtools::install_local('../prioritizr')"

.PHONY: initc clean data docs readme site test check checkwb build  install man
