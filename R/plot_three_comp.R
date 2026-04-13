globalVariables(c("V1", "V2", "col_var", "lab"))


#' Plot 3-simplex data in a the triangle contrained 2-D plane (ternary diagram)
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Plot 3-simplex data in a the triangle contrained 2-D plane (ternary diagram)
#' @param comp_data a column named  \code{data.frame} of \code{n} observations (rows). The first three columns are assumed the \code{3} compositional components (columns)
#' @param col_var_nm (default \code{NULL}) If not null and a scalar \code{character} name, a named column in \code{comp_data} corresponding to the points' values to be mapped a colour palette.
#' @param pt_alpha (default \code{0.9}) Transparency of the points plotted
#' @export
#' @details
#' Plots a \code{ggplot} 2D scatterplot of the 3-simplex data (in a 2-D triangle).
#' 
#' 
#' Currently the optional \code{col} data are assumed \code{numeric} or 
#' \code{integer} values. Currently handling of factor and nominal 
#' variables is not implemented.
#'
#'
#' @examples
#' (grid_3simplex <- (enumerate_simplex(p = 3, n = 10 - 3) + 1) / 10)
#' colnames(grid_3simplex) <- paste0("x", 1:ncol(grid_3simplex))
#' # simulate response variable based on 4-simplex ilrs
#' y <- simplex_to_ilr(grid_3simplex) %*% matrix(c(-1, 0.5), ncol = 1)
#' colnames(y) <- "outcome"
#' tern_dat <- as.data.frame(cbind(grid_3simplex, y))
#' plot_three_comp(tern_dat, col_var_nm = "outcome")
#' # No colour scale version:
#' # plot_three_comp(tern_dat)



plot_three_comp <- function(comp_data, col_var_nm = NULL, pt_alpha = 0.9) {
  
  cmp_prt_nms <- colnames(comp_data)
  if (is.null(cmp_prt_nms)) {
    cmp_prt_nms <- paste0("x", 1:3)
  }

  # linear transformation S^3 --> R^2 where
  # {(1, 0, 0), (0, 1, 0), (0, 0, 1), (1/3. 1/3, 1/3)} maps to
  #   {(0, 1/2), (-1/2, -1/2), (1/2, -1/2), (0, 0)} respectively is:
  # T(v) = Av + b where v in S^3 and T(v) in R^2 with b = (0,0) and 
  # A is as below
  A <- matrix(c(0, +1, -1, -1, +1, -1) / 2, nrow = 2)
  comp_mat <- as.matrix(comp_data[, 1:3])
  tern_coords <- as.data.frame(comp_mat %*% t(A))
  # tern_coords
  
  
  if (!is.null(col_var_nm)) {
    tern_coords[["col_var"]] <- comp_data[[col_var_nm]]
  }
  
  tern_axes <-
    tribble(
      ~V1, ~V2, ~lab, ~ndg_x, ~ndg_y,
      0, +1/2, paste0("(", cmp_prt_nms[1], " = 1, 0, 0)"), 0, 0.05,
      -1/2, -1/2, paste0("(0, ", cmp_prt_nms[2], " = 1, 0)"), 0, -0.05, 
      +1/2, -1/2, paste0("(0, 0, ", cmp_prt_nms[3], " = 1)"), 0, -0.05, 
      0, +1/2, NA, 0, 0
    )
  
  plt_gg <-
    ggplot(tern_coords, aes(x = V1, y = V2)) +
    theme_void() +
    theme(panel.background = element_rect(fill = alpha("black", 0.2))) +
    geom_polygon(
      data = tern_axes, 
      aes(x = V1, y = V2), 
      col = alpha("white", 1), 
      fill = alpha("white", 1)
    ) +
    geom_text(
      data = tern_axes, 
      aes(x = V1, y = V2, label = lab), 
      na.rm = TRUE, 
      nudge_x = tern_axes$ndg_x, 
      nudge_y = tern_axes$ndg_y
    ) +
    coord_fixed(ratio = sqrt(1/4 + 1/4)) 
  
  if (is.null(col_var_nm)) {
    plt_gg <- plt_gg + geom_point(alpha = pt_alpha)
  } else {
    plt_gg <- 
      plt_gg + 
      geom_point(aes(col = col_var), alpha = pt_alpha) +
      scale_color_viridis_c(option = "D") +
      labs(col = col_var_nm)
  }

  return(plt_gg)
  
}


