

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
#' # ref_dat
#' # real data to imitate
#' library(dplyr)
#' library(compositions)
#' data("fairclough", package = "deltacomp")
#' fc3 <-
#'   fairclough %>%
#'   mutate(pa = lpa + mpa + vpa) %>%
#'   select(sed, pa, sleep)
#' summary(fc3)
#' fc3_ref <- ilr(acomp(fc3))
#' plot(fc3)
#' plot(fc3_ref)
#' # make compositions grid loosely based on summary(fc3)
# mygrid <- rbind(
#   expand.grid(
#     c1 = seq(300, 700, 10),
#     c2 = seq(200, 500, 10),
#     c3 = seq(400, 650, 10)
#   )
# )
# mg <- subset(mygrid, rowSums(mygrid) == 1440) # 1440 min in a day
# mg_ilrs <- ilr(acomp(mg))
#
# remaing_rows <-
#   clip_to_ref_hull(data = mg_ilrs, ref_data = unclass(fc3_ref))
#
# as.matrix(fc3_ref)

clip_to_ref_hull <- function(data, ref_data, print_all = TRUE) {

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

  # convex hull vertices
  ch_v <- rd[ch_rd]
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
      ggpairs(
        .,
        upper = list(continuous = wrap("points", size = 0.1, colour = in_or_out_cols)),
        diag = list(continuous ="blankDiag"),
        lower = list(continuous = wrap("points", size = 0.1, colour = in_or_out_cols))
      ) %>%
      print(.)

  } else {

    in_or_out_cols <- c(in_or_out_cols0, in_or_out_cols[in_or_out == 1])

    rbind(
      as.data.frame(rd),
      as.data.frame(distinct_points[in_or_out == 1, ])
    ) %>%
      ggpairs(
        .,
        upper = list(continuous = wrap("points", size = 0.1, colour = in_or_out_cols)),
        diag = list(continuous ="blankDiag"),
        lower = list(continuous = wrap("points", size = 0.1, colour = in_or_out_cols))
      ) %>%
      print(.)

  }

  return(as.data.frame(distinct_points[in_or_out == 1, ]))

}









