# `simplexdat` package

## Installation

```r
install.packages(
  c("gMOIP", "geometry", "compositions", "foreach", "ggplot2", "GGally", "dplyr", "purrr")
) # required for simplexdat package

library(devtools) # see https://www.r-project.org/nosvn/pandoc/devtools.html
devtools::install_github('tystan/simplexdat')
library(simplexdat)
### see help file to run example
?mk_simplex_grid
```

## Example usage


```r
mk_simplex_grid(dim = 3)
```

