
#' Scale values in a vector to \code{[0, 1]} range
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Scale values in a vector to \code{[0, 1]} range
#' @param x a vector of numeric values
#' @param rm_na (default \code{TRUE}) should \code{NA} values be removed (almost always a good idea)
#' @export
#' @details
#' Returns a vector of equal size of \code{x} with values scaled to the range [0,1]
#' based on the min and max values of \code{x}.
#'
#' @examples
#' set.seed(1234)
#' (x <- rnorm(10))
#' scale01(x)
#' plot(x, scale01(x))


scale01 <- function(x, rm_na = TRUE) {
  min_x <- min(x, na.rm = rm_na)
  max_x <- max(x, na.rm = rm_na)
  (x - min_x) / (max_x - min_x)
}
