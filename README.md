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

If you don't have `devtools` installed but are using Windows, you can download and install the below zip file:

[simplexity_0.2.0.zip](https://github.com/tystan/simplexity/blob/master/simplexity_0.2.0.zip)


### Loading and using `simplexity`

```r
library(simplexity)

# now package loaded, see help file to run example
?mk_simplex_grid
# or create an example 4-simplex plot
example("plot_four_comp", package = "simplexity")
```

### Example usage


```r
# create a grid of evenly spaced simplex values
# number of cores = 1 faster for small computations
# remove observations that are on the edge of the simplex (rm_edges = TRUE)
grid_4simplex <- mk_simplex_grid(4, 0.2, nc = 1, rm_edges = TRUE)
colnames(grid_4simplex) <- paste0("comp", 1:ncol(grid_4simplex))
grid_4simplex
#      comp1 comp2 comp3 comp4
# [1,]   0.2   0.2   0.2   0.4
# [2,]   0.2   0.2   0.4   0.2
# [3,]   0.2   0.4   0.2   0.2
# [4,]   0.4   0.2   0.2   0.2

# simulate a fictitious response variable based on 4-simplex ilrs
y <- simplex_to_ilr(grid_4simplex) %*% matrix(c(-1, 1, 0.5), ncol = 1)
colnames(y) <- "outcome"
#         outcome
# [1,] -0.3279463
# [2,]  0.1621827
# [3,]  0.7660467
# [4,] -0.6002831

# add the response variable to the simplex data for plotting
grid_4simplex <- as.data.frame(cbind(y, grid_4simplex))
# this is an interactive/movable plotly 3D scatterplot
plot_four_comp(grid_4simplex, "comp1", "comp2", "comp3", "comp4", col = "outcome")


```

![](https://github.com/tystan/simplexity/blob/master/tetra_example.png)


