# Category vector

Convert an object containing binary (`integer`) columns into a `integer`
vector indicating the column index where each row has the highest value.

## Usage

``` r
category_vector(x)

# S3 method for class 'data.frame'
category_vector(x)

# S3 method for class 'sf'
category_vector(x)

# S3 method for class 'Spatial'
category_vector(x)

# S3 method for class 'matrix'
category_vector(x)
```

## Arguments

- x:

  `matrix`, `data.frame`, or
  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

## Value

An `integer` vector.

## Details

This function is conceptually similar to
[`base::max.col()`](https://rdrr.io/r/base/maxCol.html) except that rows
with all values equal to zero value are assigned a value of zero.

## See also

The [`base::max.col()`](https://rdrr.io/r/base/maxCol.html) provides
similar functionality.

## Examples

``` r
# create matrix with logical columns
x <- matrix(c(1, 0, 0, NA, 0, 1, 0, NA, 0, 0, 0, NA), ncol = 3)

# print matrix
print(x)
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    0    1    0
#> [3,]    0    0    0
#> [4,]   NA   NA   NA

# convert to category vector
y <- category_vector(x)

# print category vector
print(y)
#> [1]  1  2  0 NA
```
