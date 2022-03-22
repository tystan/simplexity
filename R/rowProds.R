
#' row _product_ version of rowSums()

#' @examples 
#' rowProds(matrix(1:9, ncol = 3))
#' rowProds(matrix(1:9, ncol = 1))
#' rowProds(1:9)


rowProds <- function(x) {
  if (is.null(dim(x))) {
    return(x)
  } else if (ncol(x) == 1) { # column matrix
    return(as.numeric(x))
  } else {
    return(apply(x, 1, prod))
  }
  # matrix(apply(x, 1, prod), ncol = 1)
}


