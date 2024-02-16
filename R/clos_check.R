#' Check that rows sum up to the closure value

#' @author Ty Stanford <tystan@gmail.com>
#' @description Check that rows sum up to the closure value
#' @param x \code{data.frame} or \code{matrix} with of \code{n} observations (rows) and \code{D} compositional components (columns)
#' @param clo_val a positive closure value to check each row sums to this
#' @param warn (default \code{TRUE}) should a warning about pre-closure observations not being \code{clo_val} be printed?
#' @export

#' @examples 
#' (c_dat1 <- matrix(c(rep(0.25, 4), 1:4 / 10, 1:4), byrow = TRUE, ncol = 4))
#' clos_check(c_dat1)
#' (c_dat2 <- matrix(c(rep(0.25, 4), 1:4 / 10)     , byrow = TRUE, ncol = 4))
#' clos_check(c_dat2)
#' (c_dat3 <- matrix(c(rep(0.25, 4), 1:4 / 10, 1:4), byrow = TRUE, ncol = 4))
#' clos_check(c_dat3, clo_val = 10)
#' (c_dat4 <- 3 * matrix(c(rep(0.25, 4), 1:4 / 10), byrow = TRUE, ncol = 4))
#' clos_check(c_dat4, 3)
#' 
#' 
clos_check <- function(x, clo_val = 1, warn = TRUE) {
  rwsms <- rowSums(x)
  rwsms_diffs <- abs(rwsms - clo_val)
  rwsms_not_clos <- rwsms_diffs > 1e-8
  if (any(rwsms_not_clos)) {
    if (warn) {
      cat(
        "NOTE: closure operator being applied to rows as ",
        sum(rwsms_not_clos),
        " row(s) are not closed to ",
        clo_val,
        ".\n",
        sep = ""
      )
    }
    x <- closure_rowwise(x, clo_val = clo_val)
  }
  return(x)
}


