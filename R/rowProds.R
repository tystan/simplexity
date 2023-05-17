
#' Row product function version of rowSums()

#' @author Ty Stanford <tystan@gmail.com>
#' @description Row product function version of rowSums()
#' @param x \code{data.frame} or \code{matrix} 
#' @param fast (\code{FALSE}, default) \code{logical} whether the "faster" version is used (see details)
#' @export
#' @details 
#' Returns a vector (of length equal to the number of rows in \code{x}) of the product of the elements of each row
#' 
#' Note that \code{fast = TRUE} is faster in most cases (e.g. number of columns \code{< 40}) and 
#' will use the sum of the logged absolute values (with logic to re-include 
#' signed/negative values) exponentiated back to the original scale. Testing has shown that the 
#' \code{fast = FALSE} and \code{fast = TRUE} will not differ by \code{> 1e-12} in absolute difference.
#' 
#' 
#' @examples 
#' rowProds(matrix(1:9, ncol = 3))
#' rowProds(matrix(1:9, ncol = 3), fast = TRUE) # imperceptible time diff for small datasets
#' rowProds(matrix(1:9, ncol = 1))
#' rowProds(1:9)

# faster in most cases (p < 40) but may lose some precision via the transformation
rowProds <- function(x, fast = FALSE) {
  if (is.null(dim(x))) {
    return(x)
  } else if (ncol(x) == 1) { # column matrix
    return(as.numeric(x))
  } else if (fast) {
    p <- ncol(x)
    sgns <- sign(x)
    abs_prod <- exp(rowSums(log(abs(x))))
    zero_prod <- as.integer(rowSums(sgns == 0) == 0)
    sign_prod <- ((rowSums(sgns) %% 4) == (p %% 4)) * 2 - 1
    # sign_prod <- ifelse((rowSums(sgns) %% 4) == (p %% 4), 1, -1) # same same
    # sign_prod <- (((rowSums(sgns < 0) %% 2) == 0) * 2) - 1 # slightly slower
    return(zero_prod * sign_prod * abs_prod)
  } else {
    return(apply(x, 1, prod))
  }
}


# # slower but simpler (doesn't require maths to do transformation/logic calcs)
# rowProds2 <- function(x) {
#   if (is.null(dim(x))) {
#     return(x)
#   } else if (ncol(x) == 1) { # column matrix
#     return(as.numeric(x))
#   } else {
#     return(apply(x, 1, prod))
#   }
# }
 
# # testing
# n <- 1e6
# p <- 20
# eps_ <- 1e-12
# this_x <- matrix(rnorm(n * p), ncol = p)
# this_x[1, 1] <- 0
# this_x[2, p] <- 0
# head(this_x)
# system.time(rp1 <- rowProds(this_x, fast = TRUE))
# system.time(rp2 <- rowProds(this_x, fast = FALSE))
# table(abs(rp1 - rp2) < eps_, useNA = "ifany")
# head(cbind(rp1, rp2))
# diff_calcs <- abs(rp1 - rp2) > eps_
# table(diff_calcs) # all false? (i.e. the same values)
# if (sum(diff_calcs) > 0) { # if any differences print them
#   head(cbind(rp1[diff_calcs], rp2[diff_calcs]))
# }


