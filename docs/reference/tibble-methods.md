# Manipulate tibbles

Assorted functions for manipulating
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
objects.

## Usage

``` r
# S4 method for class 'tbl_df'
nrow(x)

# S4 method for class 'tbl_df'
ncol(x)

# S4 method for class 'tbl_df'
as.list(x)
```

## Arguments

- x:

  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object.

## Details

The following methods are provided from manipulating
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
objects.

- nrow:

  `integer` number of rows.

- ncol:

  `integer` number of columns.

- as.list:

  convert to a `list`.

- print:

  print the object.

## Examples

``` r
# load tibble package
require(tibble)
#> Loading required package: tibble

# make tibble
a <- tibble(value = seq_len(5))

# number of rows
nrow(a)
#> [1] 5

# number of columns
ncol(a)
#> [1] 1

# convert to list
as.list(a)
#> $value
#> [1] 1 2 3 4 5
#> 
```
