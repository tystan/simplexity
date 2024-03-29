% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_four_comp.R
\name{plot_four_comp}
\alias{plot_four_comp}
\title{Plot 4-simplex data in a 3-D tetrahedron}
\usage{
plot_four_comp(comp_data, x1, x2, x3, x4, col = NULL, alpha = 0.9)
}
\arguments{
\item{comp_data}{a \code{data.frame} of \code{n} observations (rows) and \code{4} compositional components (columns)}

\item{x1}{\code{character} name of the first compositional component to be used that is a named column in \code{comp_data}}

\item{x2}{\code{character} name of the second compositional component to be used that is a named column in \code{comp_data}}

\item{x3}{\code{character} name of the third compositional component to be used that is a named column in \code{comp_data}}

\item{x4}{\code{character} name of the fourth compositional component to be used that is a named column in \code{comp_data}}

\item{col}{(default \code{NULL}) If not null and a scalar \code{character} name, a named column in \code{comp_data} corresponding to the points' values to be mapped a colour palette.}

\item{alpha}{(default \code{0.9}) Transparency of the points plotted}
}
\description{
Plot 4-simplex data in a 3-D tetrahedron
}
\details{
Plots a \code{plotly} 3D scatterplot of the 4-simplex data (in a 3-D tetrahedron).

The hover-labels of the points are of the original compositional data and not the 3D casrtesian coordinates.

The \code{character} values of \code{x1}, \code{x2}, \code{x3} and \code{x4} are the labels of the tetrahedron vertices.

Currently the optional \code{col} data are assumed \code{numeric} or \code{integer} values. Currently handling of factor and nominal variables is not implemented.
}
\examples{
(grid_4simplex <- mk_simplex_grid(4, 0.2, rm_edges = TRUE, nc = 1))
colnames(grid_4simplex) <- paste0("comp", 1:ncol(grid_4simplex))
# simulate response variable based on 4-simplex ilrs
y <- simplex_to_ilr(grid_4simplex) \%*\% matrix(c(-1, 1, 0.5), ncol = 1)
colnames(y) <- "outcome"
tetra_dat <- as.data.frame(cbind(y, grid_4simplex))
plot_four_comp(tetra_dat, "comp1", "comp2", "comp3", "comp4", col = "outcome")
# No colour scale version:
# plot_four_comp(tetra_dat, "comp1", "comp2", "comp3", "comp4", col = NULL)
}
\author{
Ty Stanford <tystan@gmail.com>
}
