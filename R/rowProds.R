
#' row _product_ version of rowSums()

#' @author Ty Stanford <tystan@gmail.com>
#' @description row _product_ version of rowSums()
#' @param x \code{data.frame} or \code{matrix} 
#' @export
#' @details 
#' Returns a vector (of length equal to the number of rows in \code{x})of the product of the elements of each row
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


