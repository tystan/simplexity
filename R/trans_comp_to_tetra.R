
#' Transform 4-simplex data to a 3-D plotting coordinates
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Transform 4-simplex data to a 3-D plotting coordinates
#' @param comp_dat a matrix of \code{n} observations (rows) and \code{4} compositional components (columns)
#' @param warn (default \code{FALSE}) The closure operation to 1 is applied to 
#' the observations in \code{comp_dat}, should a warning about pre-closure observations not being 1 be printed?
#' @export
#' @details
#' Returns a \code{n x 3} \code{data.frame} of \code{(x, y, z)} 3-D plotting coordinates.
#'
#' @examples
#' (grid_4simplex <- mk_simplex_grid(4, 0.2, rm_edges = TRUE, nc = 1))
#' trans_comp_to_tetra(grid_4simplex)
#' 


trans_comp_to_tetra <- function(comp_dat, warn = FALSE) {
  
  comp_dat <- clos_check(comp_dat, warn = warn)
  
  c4 <- 9 / 2
  c5 <- 5
  # n <- nrow(comp_dat)
  # note: if using matrix algebra then use c_mat
  # however, using the constant c5 saves memory
  # c_mat <- matrix(c5, nrow = n, ncol = 3)
  
  trans_mat <-
    c4 *
    matrix(
      c(
         1, -1, -1,  1,
        -1,  1, -1,  1,
         1,  1, -1, -1
      ),
      ncol = 4,
      byrow = TRUE
    )
  
  xyz <- comp_dat %*% t(trans_mat) + c5 # or + c_mat
  xyz <- as.data.frame(xyz)
  colnames(xyz) <- letters[24:26]
  
  return(xyz)
  
}
