#' Plot 4-simplex data in a 3-D tetrahedron
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Plot 4-simplex data in a 3-D tetrahedron
#' @param comp_data a \code{data.frame} of \code{n} observations (rows) and \code{4} compositional components (columns)
#' @param x1 \code{character} name of the first compositional component to be used that is a named column in \code{comp_data}
#' @param x2 \code{character} name of the second compositional component to be used that is a named column in \code{comp_data}
#' @param x3 \code{character} name of the third compositional component to be used that is a named column in \code{comp_data}
#' @param x4 \code{character} name of the fourth compositional component to be used that is a named column in \code{comp_data}
#' @param col (default \code{NULL}) If not null and a scalar \code{character} name, a named column in \code{comp_data} corresponding to the points' values to be mapped a colour palette.
#' @param alpha (default \code{0.9}) Transparency of the points plotted
#' @export
#' @details
#' Plots a \code{plotly} 3D scatterplot of the 4-simplex data (in a 3-D tetrahedron).
#' 
#' The hover-labels of the points are of the original compositional data and not the 3D casrtesian coordinates.
#' 
#' The \code{character} values of \code{x1}, \code{x2}, \code{x3} and \code{x4} are the labels of the tetrahedron vertices.
#' 
#' Currently the optional \code{col} data are assumed \code{numeric} or \code{integer} values. Currently handling of factor and nominal variables is not implemented.
#'
#'
#' @examples
#' (grid_4simplex <- mk_simplex_grid(4, 0.2, rm_edges = TRUE, nc = 1))
#' colnames(grid_4simplex) <- paste0("comp", 1:ncol(grid_4simplex))
#' # simulate response variable based on 4-simplex ilrs
#' y <- simplex_to_ilr(grid_4simplex) %*% matrix(c(-1, 1, 0.5), ncol = 1)
#' colnames(y) <- "outcome"
#' tetra_dat <- as.data.frame(cbind(y, grid_4simplex))
#' plot_four_comp(tetra_dat, "comp1", "comp2", "comp3", "comp4", col = "outcome")
#' # No colour scale version:
#' # plot_four_comp(tetra_dat, "comp1", "comp2", "comp3", "comp4", col = NULL)



plot_four_comp <- function(comp_data, x1, x2, x3, x4, col = NULL, alpha = 0.9) {
  
  obs_labs <- NULL
  if (is.null(col)) {
    col <- "col"
    comp_data[[col]] <- 1
  } else {
    obs_labs <- paste0("<br>", col, " = ", sprintf("%6.2f", comp_data[[col]]))
  }
  
  null_axis <- 
    list(
      title = "", 
      ticktext = "", 
      tickvals = "",
      # showgrid = FALSE,
      showspikes = FALSE
    )
  
  obs_labs <-
    paste0(
      obs_labs,
      "<br>", x1, " = ", comp_data[[x1]],
      "<br>", x2, " = ", comp_data[[x2]],
      "<br>", x3, " = ", comp_data[[x3]],
      "<br>", x4, " = ", comp_data[[x4]]
    )
  
  comp_mat <- as.matrix(comp_data[, c(x1, x2, x3, x4)])
  tetra_coord <- 
    cbind(
      trans_comp_to_tetra(comp_mat), 
      col = comp_data[[col]],
      obs_labs = obs_labs
    )
  
  
  vert_comp <- diag(4)
  vert_labs_txt <- c(x1, x2, x3, x4) 
  vertex_dat <- trans_comp_to_tetra(vert_comp, warn = TRUE)
  vert_labs_dat <- cbind(vertex_dat, txt = vert_labs_txt)
  colnames(vertex_dat) <- paste0("V", 1:3)
  
  edge_dat <-
    rbind(
      vertex_dat,
      vertex_dat[c(2, 4, 1, 3), ]
    )
  
  # add points of 4-simplex in tetra
  plty <- 
    plot_ly() %>%
    add_trace(
      type = "scatter3d",
      mode = "markers",
      data = tetra_coord,
      x = ~x, 
      y = ~y, 
      z = ~z, 
      # color  = ~col,
      opacity = alpha,
      hovertext = ~obs_labs,
      hoverinfo = "text",
      hoverlabel = list(
        align = "right",
        bgcolor = map_cts_to_scale(tetra_coord[["col"]])
      ),
      marker =
        list(
          # coloraxis = "coloraxis",
          color = ~col,
          showscale = TRUE,
          colorscale = "Viridis",
          colorbar = list(
            len = 0.5,
            title = list(
              text = col,
              font = list(
                size = 20
              )
            )
          )
        )
      # hovertemplate = "%{text}"
      # showlegend = FALSE
    ) 
  
  
  # create tetra edges
  plty <- 
    plty %>%
    add_trace(
      x = edge_dat[["V1"]], 
      y = edge_dat[["V2"]], 
      z = edge_dat[["V3"]], 
      type = 'scatter3d', 
      mode = 'lines+markers', 
      opacity = 1,
      line = list(color = 'black', width = 1),
      marker = list(color = 'black'),
      showlegend = FALSE
    )
  
  # label vertices
  plty <- 
    plty %>% 
    add_text(
      x = vert_labs_dat[["x"]], 
      y = vert_labs_dat[["y"]], 
      z = vert_labs_dat[["z"]], 
      text = vert_labs_dat[["txt"]],
      showlegend = FALSE
    )
  
  
  
  # remove x, y, z axes
  plty <- 
    plty %>% 
    layout(
      scene = list(
        # dragmode = "orbit",
        xaxis = null_axis,
        yaxis = null_axis,
        zaxis = null_axis
      )
    )
  
  return(plty)
  
}


