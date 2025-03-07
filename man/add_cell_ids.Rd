% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fast_aggregate.R
\name{add_cell_ids}
\alias{add_cell_ids}
\title{Add an cell id to each point}
\usage{
add_cell_ids(data_tb, grid_origin, grid_size)
}
\arguments{
\item{data_tb}{a data frame with longitude and latitude columns.}

\item{grid_origin}{a numeric. Origin of the grid.}

\item{grid_size}{a numeric. Size of the grid.}
}
\value{
a data frame with cell centroids (x_cent, y_cent) and a ID built
using the ceentroids (cell_id).
}
\description{
Given a data frame with point coordiantes and a grid, add a cell id to each
point
}
