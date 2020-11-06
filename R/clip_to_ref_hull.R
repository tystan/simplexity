# ---- libs ----



req_packages <- c("geometry", "gMOIP", "ggplot2", "GGally", "dplyr")

for (i in 1:length(req_packages)) {
  options(warn = 2, show.error.messages = FALSE) # warnings now errors
  try_load <- try({isTRUE(require(package = req_packages[i], character.only = TRUE))}, silent = TRUE)
  options(warn = 0, show.error.messages = TRUE) # back to default
  if ((class(try_load) == "try-error") | !try_load) {
    stop("\n\n####\n\nPlease install the package '", req_packages[i], "' before using this file.\n\n####\n\n")
  } else {
    cat("Sucessfully loaded package: '", req_packages[i], "'\n\n", sep = "")
  }
}

ggplot2::theme_set(ggplot2::theme_bw())


# ---- funcs ----


add_alpha <- function(col, alpha = 1) {
  apply(
    sapply(col, col2rgb) / 255,
    2, # by column
    function(x) rgb(x[1], x[2], x[3], alpha = alpha)
  )
}


# this function removes points from n x m `data` (n obs, m variables) that lie outside the 
#   m-dim convex hull of `ref_data`
# useful for containing generated/randomly data that do not extrapolate `ref_data`

# function returns the subset of `data` contained within the m-dim convex hull of `ref_data`
# a set of pairwise plots of the m-dimensions including all `data` and `ref_data` is produced with 
# * black points representing points of `ref_data` 
# * blue representing `data` within the m-dim convex hull of `ref_data`

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









