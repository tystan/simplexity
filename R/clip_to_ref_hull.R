globalVariables(c("i", "."))

#' Limit `data` to points contained within the m-dim convex hull of a reference set of data
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Limit \code{data} to points contained within the m-dim convex hull of a reference set of data
#' @param data numeric n1 x m data (n1 obs, m variables) that is coersable to matrix class
#' @param ref_data another m-dim dataset (n2 x m data, i.e., n2 obs, m variables) that is coersable to
#'     matrix class. This dataset is the reference dataset which creates the m-dim convex hull that
#'    \code{data} is tested against.
#' @param print_all (\code{TRUE}, default) produces pairwise variable plots including points
#'    in \code{data} that have been removed from the returned dataset.
#'    \code{FALSE} only prints the remaining data points in \code{data} and the data
#'    points in \code{data_ref}.
#' @param point_size point size in GGally:ggparis() plot (default \code{2} to enlarge points as output is multifaceted)
#' @export
#' @details
#' \code{clip_to_ref_hull()} removes points from n1 x m \code{data} (n1 obs, m variables) that lie outside the
#'   m-dim convex hull of \code{ref_data}.
#'
#' This function may be useful for containing generated/randomly data that do not extrapolate \code{ref_data}
#'
#' Returned is the subset of \code{data} contained within the m-dim convex hull of \code{ref_data}
#'
#' A set of pairwise plots of the m-dimensions including all \code{data} (subject to \code{print_all}
#' argument value) and \code{ref_data} is produced, with
#'
#' \itemize{
#'   \item black points representing points of \code{ref_data}
#'   \item blue representing \code{data} within the m-dim convex hull of \code{ref_data}
#' }
#'
#' @examples
#' library(dplyr)
#' # real data to imitate
#' data("fairclough", package = "codaredistlm") # see github.com/tystan/codaredistlm
#' fc3 <-
#'   fairclough %>%
#'   mutate(pa = lpa + mpa + vpa) %>%
#'   select(sed, pa, sleep)
#' summary(fc3)
#' fc3_ref <- simplex_to_ilr(as.matrix(fc3))
#' plot(fc3)
#' plot(fc3_ref)
#' # make compositions grid loosely based on summary(fc3)
#' # 10 min intervals
#' mygrid <- mk_simplex_grid(3, 10 / 1440, rm_edges = TRUE, nc = 1) * 1440 
#' colnames(mygrid) <- c("sed", "pa", "sleep")
#' mygrid <- as_tibble(mygrid)
#' head(mygrid)
#' kp_i <- 
#'   with(mygrid, as.logical(
#'     (  sed >= 300 &   sed <= 700) &
#'     (   pa >= 200 &    pa <= 500) &
#'     (sleep >= 400 & sleep <= 650) 
#'   ))
#' mygrid <- mygrid[kp_i, ]
#' mg_ilrs <- simplex_to_ilr(as.matrix(mygrid))
#' head(mg_ilrs)
#' remaining_rows <- clip_to_ref_hull(data = mg_ilrs, ref_data = fc3_ref)



clip_to_ref_hull <- function(data, ref_data, print_all = TRUE, point_size = 2) {

  data <- as.matrix(data)
  rd <- as.matrix(ref_data)

  if (ncol(data) != ncol(ref_data)) {
    stop("data and ref_data must have the same number of dimensions")
  }

  if (ncol(data) > nrow(data)) {
    warning("less observations than dimensions which is problematic")
  }

  # indexes of n-dim hull
  ch_rd <- geometry::convhulln(rd)
  ch_rd <- foreach(i = 1:ncol(ch_rd), .combine = c) %do% {c(ch_rd[, i])}
  ch_rd <- sort(unique(ch_rd))
    
  # convex hull vertices
  ch_v <- rd[ch_rd, ]
  # limit to distinct points
  ch_v <- as.data.frame(ch_v) %>% distinct() %>% as.matrix()

  distinct_points <- as.data.frame(data) %>% distinct() %>% as.matrix()

  in_or_out <-
    gMOIP::inHull(
      pts = distinct_points,
      vertices = ch_v
    )
  # -1s are out, but change them to 2s (so we have 1s and 2s)
  in_or_out[in_or_out < 0] <- 2
  # 0s are on the hull so make them "included"
  in_or_out[in_or_out == 0] <- 1

  cat("These are the number of points in data that are in the reference hull (==1) and not (==2)\n")
  print(table(in_or_out))

  in_or_out_cols <- add_alpha(c("dodgerblue", "orange")[in_or_out], 0.25)
  in_or_out_cols0 <- add_alpha(rep("black", nrow(rd)), 1)

  # plot data
  if (print_all) {

    in_or_out_cols <- c(in_or_out_cols0, in_or_out_cols)

    rbind(
      as.data.frame(rd),
      as.data.frame(distinct_points)
    ) %>%
      {
        ggpairs(
          .,
          upper = list(continuous = wrap("points", size = point_size, colour = in_or_out_cols)),
          diag = list(continuous ="blankDiag"),
          lower = list(continuous = wrap("points", size = point_size, colour = in_or_out_cols))
        ) + 
        theme_bw()
      } %>%
      print(.)

  } else {

    in_or_out_cols <- c(in_or_out_cols0, in_or_out_cols[in_or_out == 1])

    rbind(
      as.data.frame(rd),
      as.data.frame(distinct_points[in_or_out == 1, ])
    ) %>%
      {
        ggpairs(
          .,
          upper = list(continuous = wrap("points", size = point_size, colour = in_or_out_cols)),
          diag = list(continuous ="blankDiag"),
          lower = list(continuous = wrap("points", size = point_size, colour = in_or_out_cols))
        ) + 
          theme_bw()
      } %>%
      print(.)

  }

  return(as.data.frame(distinct_points[in_or_out == 1, ]))

}









