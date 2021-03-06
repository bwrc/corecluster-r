% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities_corecluster.R
\name{co_occurrence_prob_single_parallel}
\alias{co_occurrence_prob_single_parallel}
\title{Estimate co-occurence probability Pij of two data items x1 and x2. The function is
parallellized and utilizes (number-of-cores - 1) parallel computations.}
\usage{
co_occurrence_prob_single_parallel(x1, x2, data_function, cluster_function,
  iter = 1000)
}
\arguments{
\item{x1}{Data item i.}

\item{x2}{Data item j.}

\item{data_function}{A function, such that data_function() returns n-2 resampled data points.}

\item{cluster_function}{A clustering function that returns a set of cluster labels.}

\item{iter}{The number of iterations. Optional. Default is 1000.}
}
\value{
The co-occurence probability of x1 and x2; i.e., the
probability that data items x1 and x2 occur in the same cluster.
}
\description{
Estimate co-occurence probability Pij of two data items x1 and x2. The function is
parallellized and utilizes (number-of-cores - 1) parallel computations.
}

