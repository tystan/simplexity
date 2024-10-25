


#' Create a grid on a m-dim simplex
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Create a grid on a m-dim simplex, enumerating all points spaced by \code{step_size}
#' @param p the dimension of the simplex. This will be the number of columns in the returned data
#' @param n integer compositions/axes to be enumerated
#' @param verbose (TRUE, default) print output matrix dimensions and object size in MB to console
#' @export



enumerate_simplex <- function(p, n, verbose = TRUE) {
  
  n_points <- function(D, m) {
    as.integer(round(prod((D + m - 1):(m + 1)) / factorial(D - 1)))
  }
  
  p <- as.integer(p)
  n <- as.integer(n)
  
  if (p < 1L) {
    warning("p must be an integer of least 1")
    return(NULL)
  } else if (n < 0L) {
    warning("n must be a non-negative integer")
    return(NULL)
  } else if (n == 0L) {
    warning("As n == 0, returning the trivial result")
    return(matrix(0L, nrow = 1, ncol = p))
  }
  
  count <- n_points(p, n)
  
  if (verbose) {
    message("There will be ", count, " rows and ", p, " cols in the returned matrix")
    out_tmp <- matrix(0L, nrow = count, ncol = p)   
    out_tmp_sz <- format(object.size(out_tmp), "MB") 
    rm(list = "out_tmp")
    message("This matrix will be approximately ", out_tmp_sz, " in memory")
  }
  
  return(enumerate_simplex_cpp(p, n, count))
  
}
