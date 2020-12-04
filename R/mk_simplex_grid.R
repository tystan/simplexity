

#' Create a grid on a m-dim simplex
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Create a grid on a m-dim simplex, enumerating all points spaced by \code{step_size}
#' @param dim the dimension of the simplex. This will be the number of columns in the returned data
#' @param step_size (default 0.1) the increments in each of the compositions/axes to be enumerated
#' @export
#' @details
#' returns a matrix with \code{dim} columns and n rows that is increasingly large for increasing \code{dim}.
#' Below is a table of the row numbers for a \code{step_size} of 0.1 (default) and \code{dim} dimensions
#'
#' \tabular{rrrr}{
#'   \code{dim} \tab \code{step_size} \tab n rows \tab time taken to run (sec) on i5-8400 \cr
#'   2 \tab 0.1 \tab 11 \tab 0.004 \cr
#'   3 \tab 0.1 \tab 66 \tab 0.039  \cr
#'   4 \tab 0.1 \tab 286 \tab 0.248  \cr
#'   5 \tab 0.1 \tab 1001 \tab 1.131  \cr
#'   6 \tab 0.1 \tab 3003 \tab 4.168  \cr
#'   7 \tab 0.1 \tab 8008 \tab 13.549  \cr
#'   8 \tab 0.1 \tab 19448 \tab  38.434 \cr
#'   9 \tab 0.1 \tab 43758 \tab 97.613
#' }
#'
#'
#' @examples
#' mk_simplex_grid(2)
#' mk_simplex_grid(2, 0.05)
#' nrow(mk_simplex_grid(2))
#' system.time(mk_simplex_grid(2))
#'
#' mk_simplex_grid(5, 0.5)


mk_simplex_grid <- function(dim, step_size = 0.1) {

  .mk_var(dim - 1, 1, step_size)

}


.mk_var <- function(remain_var, remain_comp, step_size) {

  if (remain_var < 1) {

    return(remain_comp)

  } else {

    this_var <- seq(0, remain_comp, by = step_size)

    return(
      foreach(i = seq_along(this_var), .combine = rbind) %do% {

        cbind(
          this_var[i],
          .mk_var(
            remain_var - 1,
            max(0, remain_comp - this_var[i]),
            step_size
          )
        )

      }
    )

  }
}


