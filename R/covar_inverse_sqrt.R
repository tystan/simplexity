





#' Calculate the inverse square-root of the variance-covariance matrix
#'
#' @param covar_mat a positive-definite, square (n by n) matrix
#' @param verbose provide working (FALSE, default)
#' 
#' @importFrom expm sqrtm
#' @importFrom utils write.table
#' @return
#' Find the matrix U such that
#' 
#' \code{inverse(covar_mat)} \eqn{= U U}{= U U}
#' 
#' @note
#' Uses \code{expm::sqrtm()} so should be optimised for speed over 
#' eigen decompositions manually, i.e., the implementation in 
#' \code{covar_to_power(., pow = -1/2)} 
#' 
#' @export
#'
#' @examples
#' A <- diag(2)
#' covar_inverse_sqrt(A)
#' covar_inverse_sqrt(A, verbose = TRUE)
#' covar_to_power(A, pow = -1/2, verbose = TRUE)
#' all(abs(covar_inverse_sqrt(A) %*% covar_inverse_sqrt(A) - solve(A)) < 1e-12)
#' all(abs(covar_to_power(A, pow = -1/2) %*% covar_to_power(A, pow = -1/2) - solve(A)) < 1e-12)
#' 
#' B <- diag(c(1, 4))
#' covar_inverse_sqrt(B)
#' covar_inverse_sqrt(B, verbose = TRUE)
#' covar_to_power(B, pow = -1/2, verbose = TRUE)
#' all(abs(covar_inverse_sqrt(B) %*% covar_inverse_sqrt(B) - solve(B)) < 1e-12)
#' all(abs(covar_to_power(B, pow = -1/2) %*% covar_to_power(B, pow = -1/2) - solve(B)) < 1e-12)
#' 
#' C <- matrix(c(4, -2, -2, 4), nrow = 2)
#' covar_inverse_sqrt(C)
#' covar_inverse_sqrt(C, verbose = TRUE)
#' covar_to_power(C, pow = -1/2, verbose = TRUE)
#' all(abs(covar_inverse_sqrt(C) %*% covar_inverse_sqrt(C) - solve(C)) < 1e-12)
#' all(abs(covar_to_power(C, pow = -1/2) %*% covar_to_power(C, pow = -1/2) - solve(C)) < 1e-12)
#' 

covar_inverse_sqrt <- function(covar_mat, verbose = FALSE) {
  
  inv_mat <- solve(covar_mat)
  return_mat <- expm::sqrtm(inv_mat)
  
  if (verbose) {
    print_mat <- function(x) {
      message(
        write.table(
          x, 
          file = "", sep = "\t", row.names = FALSE, col.names = FALSE
        )
      )
    }
    message("Matrix multiplication of U %*% U (U is the matrix being returned) is equal to the inverse of the input:")
    print_mat(inv_mat)

  }
  
  return(return_mat)
  
}


