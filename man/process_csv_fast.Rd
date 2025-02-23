% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fast_aggregate.R
\name{process_csv_fast}
\alias{process_csv_fast}
\title{Process CSV files}
\usage{
process_csv_fast(file_path, grid_origin, grid_size)
}
\arguments{
\item{file_path}{a character. Path to a CSV file.}

\item{grid_origin}{a numeric. Origin of the aggregation grid.}

\item{grid_size}{a numeric. Size of the aggregation grid.}
}
\value{
a data frame (tibble).
}
\description{
Read a CSV file representing points and aggregate them into a regular grid.
}
