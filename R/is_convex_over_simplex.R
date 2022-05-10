globalVariables(c("f_ax_f_by_", "f_axby_", "lhs_larger"))



#' Determine if a suppled function is convex
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Function to evaluate whether \code{f(ax + by) <= a f(x) + b f(y)} is true for all
#'  \code{x,y} in a function's domain and
#   \code{a + b = 1} with \code{a,b >= 0}. i.e., determine if a suppled `f()` is convex.
#' @param func a function with a single input that is a vector that is of dimension \code{x_dim} (see below)
#' @param x_dim the dimension of the input required
#' @param n_rep (default = 10000) the number of randomly generated \code{(x,y,a,b)}-tuples generated to
#'     assess \code{f(ax + by) <= a f(x) + b f(y)}
#' @param seed_ (default: NULL) a seed for the random number generation. If NULL, no seed set and is non-reproducible
#' @export
#' @details
#' # Function to evaluate whether \code{f(ax + by) <= a f(x) + b f(y)} is true for all \code{x,y} in a function's domain and
#'   \code{a + b = 1} with \code{a,b >= 0}. i.e., determine if a suppled \code{f()} is convex.
#' The returned value is an approximation in that not all x,y,a,b are enumerated. Confidence in a "true"
#'   result increases as the number of repititions increases.
#'
#' Either \code{TRUE} or \code{FALSE} is returned. Additionally details of is_convex's findings is printed to console.
#'
#' It is assumed that the input of the function is a \code{x_dim}-dimensional composition
#' (that is sums to 1 and elements are non-negative)
#' Only a returned value of \code{FALSE} can be consided a difinitive answer to whether convexity exists as a counter example
#'   of \code{f(ax + by) <= a f(x) + b f(y)} has been found (analogous to proof by contradiction)
#' If \code{TRUE} is returned it just means no counter examples have been found

#' @examples
#' example_fun_1 <- function(x) {
#'  -sum(log(x))
#' }
#' # 2-D check, takes ~10 sec (to reduce time use n_rep = 1e4 or similar)
#' is_convex_over_simplex(example_fun_1, 2, n_rep = 1e4, seed_ = 1234)
#' # 4-D check
#' is_convex_over_simplex(example_fun_1, 4, n_rep = 1e3, seed_ = 1234)



is_convex_over_simplex <- function(func, x_dim, n_rep = 1e4, seed_ = NULL) {

  if (!is.null(seed_)) set.seed(seed_)

  if (x_dim < 2) {
    warning(
      "Having a 1-dim composition means each x value will have been a scalar value of 1 repeated ",
      n_rep,
      " times."
    )
  }

  # make list of x, y compositions where each element is a x_dim-vector
  x <- map(replicate(n_rep, runif(x_dim), simplify = FALSE), function(x) x / sum(x))
  # map_dbl(x, function(x) sum(x)) # check
  y <- map(replicate(n_rep, runif(x_dim), simplify = FALSE), function(x) x / sum(x))
  # map_dbl(y, function(x) sum(x)) # check

  # generate an a between [0,1] and make b = 1 - a
  a <- runif(n_rep)
  b <- 1 - a

  # calculate lhs = f(ax + by) and rhs = a f(x) + b f(y)
  axby_lst <-
    tibble(a = a, x = x, b = b, y = y) %>%
    mutate(
      f_axby_ = pmap_dbl(list(a, x, b, y), ~ func(..1 * ..2 + ..3 * ..4)),
      f_ax_f_by_ = pmap_dbl(list(a, x, b, y), ~ func(..1 * ..2) + func(..3 * ..4)),
      lhs_larger = f_axby_ > f_ax_f_by_
    )

  axby_lst <-
    axby_lst %>%
    dplyr::filter(lhs_larger)

  if (nrow(axby_lst) > 0) {
    # take the first occurance as an example to print
    x_str <- paste0("x=", paste(axby_lst$x[1], collapse= ", "))
    y_str <- paste0("y=", paste(axby_lst$y[1], collapse= ", "))
    a_str <- paste0("a=", axby_lst$a[[1]])
    b_str <- paste0("b=", axby_lst$b[[1]])
    cat("### Returning FALSE as randomly generated values:\n")
    cat(x_str, "\n", y_str, "\n", a_str, "\n", b_str, "\n", sep = "")
    cat("### Produce the following:\n")
    cat(
      "f(a * x + b * y) = ", axby_lst$f_axby_[[1]],
      " > ",
      "a * f(x) + b * f(y) = ", axby_lst$f_ax_f_by_[[1]],
      "\n\n", sep = ""
    )
    return(FALSE)

  } else {

    cat("f(ax + by) <= a f(x) + b f(y) was true after", n_rep, "generated (x,y,a,b)-tuples\n\n")
    return(TRUE)

  }

}

#
# # example usage #1
#
# example_fun_1 <- function(x) {
#   - sum(log(x))
# }
#
# # 1-D check, takes ~10 sec (to reduce time use n_rep = 1e4 or similar)
# is_convex_over_simplex(example_fun_1, 1, n_rep = 1e6, seed_ = 1234)
#
# # 10-D check
# is_convex_over_simplex(example_fun_1, 10, n_rep = 1e4, seed_ = 1234)
#
#
# # example usage #2
#
# example_fun_2 <- function(x) {
#   sum(log(x))
# }
#
# # 1-D check, takes ~10 sec
# is_convex_over_simplex(example_fun_2, 1, n_rep = 1e6, seed_ = 1234)
#
# # 3-D check (should return FALSE)
# is_convex_over_simplex(example_fun_2, 3, n_rep = 1e4, seed_ = 123445)

