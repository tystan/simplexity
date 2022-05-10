#' Apply closure operator over rows

#' @author Ty Stanford <tystan@gmail.com>
#' @description Apply closure operator over rows
#' @param y \code{data.frame} or \code{matrix} with of \code{n} observations (rows) and \code{D} compositional components (columns)
#' @param clo_val a positive value for each row to be closed to (i.e., each rows sums to this)
#' @export
#'
#' @examples
#' (c_dat1 <- matrix(c(rep(0.25, 4), 1:4 / 10, 1:4), byrow = TRUE, ncol = 4))
#' row_wise_closure(c_dat1)
#' 
row_wise_closure <- function(y, clo_val = 1) {
  clo_val * t(apply(y, 1, function(x) x / sum(x)))
}
