


#' Calculate the real valued power of the variance-covariance matrix
#'
#' @param covar_mat a positive-definite, square (n by n) matrix
#' @param pow the power to calculate
#' @param verbose provide working (FALSE, default)
#'
#' @return
#' 
#' Used the eigen decomposition that
#' 
#' \code{covar_mat} \eqn{= U \Lambda U^T}{= U \Lambda U^T}
#' 
#' when 
#' 
#' \code{covar_mat} a positive-definite, square (n by n) matrix.
#' 
#' Therefore
#' 
#' \code{covar_mat ** pow} \eqn{= U \Lambda^{pow} U^T}{= U \Lambda^{pow} U^T}
#'
#' Note 1: should also satisfy \code{(covar_mat ** pow) ** 1/pow = covar_mat}
#' Note 2: an implementation using  expm::power() or Matrix::chol() would 
#' probably be quicker/better
#' 
#' @export
#'
#' @examples
#' A <- diag(2)
#' covar_to_power(A, pow = 2, verbose = TRUE)
#' covar_to_power(A, pow = -1/2, verbose = TRUE)
#' covar_inverse_sqrt(A, verbose = TRUE)
#' 
#' B <- diag(c(1, 4))
#' covar_to_power(B, pow = 2, verbose = TRUE)
#' covar_to_power(B, pow = 1/2, verbose = TRUE)
#' covar_to_power(B, pow = -1/2, verbose = TRUE)
#' covar_inverse_sqrt(B, verbose = TRUE)
#' 
#' C <- matrix(c(4, -2, -2, 4), nrow = 2)
#' covar_to_power(C, pow = 2, verbose = TRUE)
#' covar_to_power(C, pow = 1/2, verbose = TRUE)
#' covar_to_power(C, pow = -1/2, verbose = TRUE)
#' covar_inverse_sqrt(C, verbose = TRUE)
#' covar_to_power(covar_to_power(C, pow = -1/2), pow = -2)
#' covar_to_power(covar_to_power(C, pow = 2), pow = 1/2)
#' covar_to_power(covar_to_power(C, pow = -1/3), pow = -3)
#' covar_to_power(covar_to_power(C, pow = +1/3), pow = +3)
#' 
covar_to_power <- function(covar_mat, pow = 1L, verbose = FALSE) {
  
  return_mat <- covar_mat
  
  if (pow != 1L) { # done otherwise
    
    print_mat <- function(x) {
      message(
        write.table(
          x, 
          file = "", sep = "\t", row.names = FALSE, col.names = FALSE
        )
      )
    }
    
    e_decomp <- eigen(covar_mat, only.values = FALSE, symmetric = FALSE)
    if (verbose) {
      message("Eigen vecs below")
      print_mat(e_decomp$vectors)
      message("Eigen vals below")
      print_mat(t(e_decomp$values))
    }
    
    V <- e_decomp$vectors 
    Lam_p <- diag(e_decomp$values^pow)
    
    return_mat <- V %*% Lam_p %*% t(V)
    
    
  }
  
  return(return_mat)
  
}



