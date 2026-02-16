# Multi-objective optimization tutorial

## Introduction

3 paragraphs: - point of tutorial - multi-objective general + concepts
of pareto frontier - vignette content

Use Washington data around 300 sp -\> assign realistic categories

Explain some more stuff on Pareto front here but the jump straight to
three objective examples Add graph for pareto front here to explain
concepts here and talk about trade-offs

## Usage

### Single-objective problem

``` r
# load packages
library(prioritizr)
library(terra)
```

    ## terra 1.8.93

    ## 
    ## Attaching package: 'terra'

    ## The following object is masked from 'package:prioritizr':
    ## 
    ##     rescale

    ## The following objects are masked from 'package:testthat':
    ## 
    ##     compare, describe

``` r
library(tibble)

# set seed for reproducibility
set.seed(500)
```

``` r
# import data
con_cost <- get_sim_pu_raster()
keystone_spp <- get_sim_features()[[1:3]]
iconic_spp <- get_sim_features()[[4:5]]

# set budget
con_budget <- terra::global(con_cost, "sum", na.rm = TRUE)[[1]] * 0.3
```

### Multi-objective problem

### Weighted-sum approach

### Relative constraint (hierarchical) approach

### Three objective functions

### Multiple objectives in multiple zones (EXTRA VIGNETTE FOR THIS)

## Conclusion

## References
