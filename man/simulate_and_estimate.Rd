% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_and_estimate.R
\name{simulate_and_estimate}
\alias{simulate_and_estimate}
\title{Simulate Response Data and Estimate IRT Parameters}
\usage{
simulate_and_estimate(seed, a, b, n_students = 500, n_items = 12)
}
\arguments{
\item{seed}{Integer seed for reproducibility.}

\item{a}{Matrix of discrimination parameters for items.}

\item{b}{Vector of difficulty parameters for items.}

\item{n_students}{Number of students to simulate responses for.}

\item{n_items}{Number of items in the simulation.}
}
\value{
A list containing theta results, item parameter results, and correlation results.
}
\description{
This function simulates item responses for a given set of item parameters and estimates
parameters using unidimensional (UIRT) and multidimensional (MIRT) IRT models.
}
\examples{
params <- generate_item_par(12)
simulate_and_estimate(seed = 1, a = params$a, b = params$b, n_students = 500, n_items = 12)
}
