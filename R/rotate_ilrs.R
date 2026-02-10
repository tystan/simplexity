




#' Title
#'
#' @param ilr_dat a \code{n} times \code{D-1} data.frame or matrix of ilrs using basis matrix \code{from_basis}
#' @param from_basis the \code{D} times \code{D-1} basis matrix used to generate \code{ilr_dat}
#' @param to_basis the \code{D} times \code{D-1} basis matrix wanting to transform \code{ilr_dat} to
#'
#' @return 
#' an equivalent ilr expression of \code{ilr_dat} using basis matrix \code{to_basis} (a \code{n} times \code{D-1} matrix of ilrs) 
#' @export
#'
#' @examples
#' (comp_dat <- matrix(c(3, 3, 4, 3, 4, 3, 1, 2, 7, 1, 7, 2), byrow = TRUE, ncol = 3))
#' (b0 <- mk_vt(3, normalise = TRUE))
#' b1 <- b0
#' b1[, 1] <- -b1[, 1]
#' b1 <- -b1[3:1,]
#' b1
#' ilr_b0 <- log(comp_dat) %*% b0
#' ilr_b1 <- log(comp_dat) %*% b1
#' stopifnot(all(abs(ilr_b1 - rotate_ilrs(ilr_b0, b0, b1) < 1e-12)))
#' closure_rowwise(exp(rotate_ilrs(ilr_b0, b0, b1) %*% t(b1)), clo_val = 10)


rotate_ilrs <- function(ilr_dat, from_basis, to_basis) {
  
  get_rotate_mat <- function(from_v, to_v) {
    return(t(to_v) %*% from_v)
  }
  
  R <- get_rotate_mat(from_basis, to_basis)
  
  return(as.matrix(ilr_dat) %*% t(R))
  
}






