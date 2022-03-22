

#' Convert \eqn{n \times (D-1)} ilr-space data back to \eqn{n \times D} simplex data  

#' @examples 
#' grid_3simplex <- mk_simplex_grid(dim = 3, step_size = 0.1, nc = 1, rm_edges = TRUE) 
#' head(grid_3simplex)
#' # transform to 2-D ilr space using v = t(mk_vt(3)) basis
#' grid_2ilr <- simplex_to_ilr(grid_3simplex) 
#' head(grid_2ilr)
#' plot(grid_2ilr[, 1], grid_2ilr[, 2], xlab = "ilr1", ylab = "ilr2")
#' # return to simplex
#' grid_back_to_simplex <- ilr_to_simplex(grid_2ilr)
#' # same as originasl data?
#' head(grid_back_to_simplex)
#' head(grid_3simplex)



ilr_to_simplex <- function(ilr_dat, clo_val = 1) {
  
  D_1 <- ncol(ilr_dat)
  V <- t(mk_vt(D_1 + 1, normalise = TRUE))
  
  simplex_dat <- exp(ilr_dat %*% V)
  simplex_dat <- row_wise_closure(simplex_dat, clo_val = clo_val)
  
  return(as.matrix(simplex_dat))
  
}

