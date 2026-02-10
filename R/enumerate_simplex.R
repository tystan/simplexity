


#' Create a grid on a m-dim simplex
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Create a grid on a m-dim simplex, enumerating all points spaced by \code{step_size}
#' @param p the dimension of the simplex. This will be the number of columns in the returned data
#' @param n integer compositions/axes to be enumerated
#' @param verbose (TRUE, default) print output matrix dimensions and object size in MB to console
#' @export
#' @examples
#' # If we want D=4 compositional parts that enumerate the day into 20% chunks (5 non zero values)
#' # We can do the following
#' D <- 4; n_chunks <- 5
#' head(enumerate_simplex(D, n = n_chunks))
#' # OR if the closure value is 1 then use
#' head(enumerate_simplex(D, n = n_chunks) / n_chunks)
#' 
#' # if you want the lattice without edge values (i.e., no zeros)
#' # you can enumerate D less chunks then add one chunk to every enumerated value
#' enumerate_simplex(D, n = n_chunks - D) + 1
#' # long way if you want to check
#' print(c(class(latt_no_0), class(latt_no_0[1, ]))) # matrix of integers
#' latt_with_0 <- enumerate_simplex(D, n = n_chunks)
#' latt_0_rows <- apply(latt_with_0 == 0L, 1, any) # by row, if any 0, then 0 in row
#' latt_with_0[!latt_0_rows, ]  # remove rows with 0s, looks the same with more steps!



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
