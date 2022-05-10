
#' Map numeric values to corresponding viridis-D scale palette
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Map numeric values to corresponding viridis-D scale palette
#' @param x a vector of numeric values
#' @param alpha transparency of the returned colours (default \code{0.9})
#' @export
#' @details
#' Returns a vector of equal size of \code{x} with viridis-D scale hex colour 
#' values (with the min and max values of \code{x} defining the range of the palette).
#' 
#' @examples
#' set.seed(1234)
#' (x <- rnorm(10))
#' x_scaled <- scale01(x)
#' x_cols <- map_cts_to_scale(x, alpha = 0.8)
#' plot(x, x_scaled, col = x_cols, pch = 16, cex = 3)


map_cts_to_scale <- function(x, alpha = 0.9) {
  x <- round(100 * scale01(x), 0) + 1
  return(get_viri_scale(alpha = alpha)[x])
}
