# `simplexity` package


[![DOI](https://zenodo.org/badge/310469213.svg)](https://zenodo.org/badge/latestdoi/310469213)


Hopefully taking out the complexity of using the simplex.

The `simplexity` package contains functions to generate, manipulate and plot data on the simplex

## Getting started

### Installation

(platform independent) If you have `devtools` installed you can use:
```r
library(devtools) # see https://www.r-project.org/nosvn/pandoc/devtools.html
devtools::install_github('tystan/simplexity') # see DESCRIPTION file for required and suggested packages
```
(it will compile some C++ code contained in the package for your platform)

If you don't have `devtools` installed but are using Windows, you can download and install the below zip file:

[simplexity_0.2.2.zip](https://github.com/tystan/simplexity/blob/master/simplexity_0.2.2.zip)
(C++ code is pre-compiled, no compiler required)

### Loading and using `simplexity`

```r
library(simplexity)

# now package loaded, see help file to run example
?enumerate_simplex
# or create an example 4-simplex plot
example("plot_four_comp", package = "simplexity")

# or look at the ellipsoid fencing vignette
vignette("ellipsoid-fencing", package = "simplexity")
```

### Example usage


```r
# create a grid of evenly spaced simplex values
# remove observations that are on the edge of the simplex (see examples of `enumerate_simplex()`)
D <- 4; n_chunks <- 5;
grid_4simplex <- enumerate_simplex(D, n_chunks - D) + 1
# There will be 4 rows and 4 cols in the returned matrix
# This matrix will be approximately 0 Mb in memory
grid_4simplex <- grid_4simplex / n_chunks # apply closure row sums = 1
colnames(grid_4simplex) <- paste0("comp", 1:ncol(grid_4simplex))
grid_4simplex
#      comp1 comp2 comp3 comp4
# [1,]   0.4   0.2   0.2   0.2
# [2,]   0.2   0.4   0.2   0.2
# [3,]   0.2   0.2   0.4   0.2
# [4,]   0.2   0.2   0.2   0.4

# simulate a fictitious response variable based on 4-simplex ilrs
y <- simplex_to_ilr(grid_4simplex) %*% matrix(c(-1, 1, 0.5), ncol = 1)
colnames(y) <- "outcome"
#         outcome
# [1,] -0.6002831
# [2,]  0.7660467
# [3,]  0.1621827
# [4,] -0.3279463

# add the response variable to the simplex data for plotting
grid_4simplex <- as.data.frame(cbind(y, grid_4simplex))
# this is an interactive/movable plotly 3D scatterplot
plot_four_comp(grid_4simplex, "comp1", "comp2", "comp3", "comp4", col = "outcome")


```

![](https://github.com/tystan/simplexity/blob/master/tetra_example.png)


