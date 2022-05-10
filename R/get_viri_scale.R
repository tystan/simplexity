#' Create a colour scale/palette based on the viridis-D scale
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Create a colour scale/palette based on the viridis-D scale
#' @param n number of colours in the palette (default \code{101}) 
#' @param alpha transparency of the returned colours (default \code{1})
#' @export
#' @details
#' Returns a vector of length \code{n} with hex colour values that can be used for plotting.
#'
#' @examples
#' get_viri_scale(4)
#' get_viri_scale(4, 0.5) # see-through
#' 


get_viri_scale <- function(n = 101, alpha = 1) {
  viridis(n = n, alpha = alpha, option = "D")
}
