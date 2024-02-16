

#' Calculate the geomtric mean down columns

#' @author Ty Stanford <tystan@gmail.com>
#' @description Calculate the geomtric mean down columns in mtrix/data.frame
#' @param x \code{data.frame} or \code{matrix} with of \code{n} observations (rows) and \code{D} compositional components (columns)
#' @export
#'
#' @examples
#' (c_dat1 <- matrix(c(rep(0.25, 4), 1:4 / 10, 1:4), byrow = TRUE, ncol = 4))
#' geo_mean_colwise(c_dat1)
#' 


geo_mean_colwise <- function(x) {
  if (is.null(dim(x))) {
    return(x)
  } else if (nrow(x) == 1) { # one row
    return(as.numeric(x))
  } else {
    n <- nrow(x)
    return(apply(x ^ (1 / n), 2, prod))
  }
}















