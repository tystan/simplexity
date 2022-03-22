#' Convert \eqn{n \times D} simplex data to \eqn{n \times (D-1)} ilr-space data

#' @examples 
#' grid_3simplex <- mk_simplex_grid(dim = 3, step_size = 0.1, rm_edges = TRUE) 
#' head(grid_3simplex)
#' # transform to 2-D ilr space using v = t(create_vt(3)) basis
#' grid_2ilr <- simplex_to_ilr(grid_3simplex) 
#' head(grid_2ilr)
#' plot(grid_2ilr[, 1], grid_2ilr[, 2], xlab = "ilr1", ylab = "ilr2")


simplex_to_ilr <- function(simplex_dat) {
  
  D <- ncol(simplex_dat)
  VT <- create_vt(D, normalise = TRUE)
  
  ilr_dat <- log(simplex_dat) %*% VT
  
  return(as.matrix(ilr_dat))
  
}


