# `simplexity` package

Hopefully taking out the complexity of using the simplex.

## Installation

```r
library(devtools) # see https://www.r-project.org/nosvn/pandoc/devtools.html
devtools::install_github('tystan/simplexity') # see DESCRIPTION file for required and suggested packages
library(simplexity)

# now package loaded, see help file to run example
?mk_simplex_grid
```

## Example usage


```r
mk_simplex_grid(dim = 3, step_size = 0.5, nc = 1) # number of cores = 1 faster for small computations
#      [,1] [,2] [,3]
# [1,]  0.0  0.0  1.0
# [2,]  0.0  0.5  0.5
# [3,]  0.0  1.0  0.0
# [4,]  0.5  0.0  0.5
# [5,]  0.5  0.5  0.0
# [6,]  1.0  0.0  0.0
```

