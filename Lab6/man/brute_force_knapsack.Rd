% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brute_force_knapsack.R
\name{brute_force_knapsack}
\alias{brute_force_knapsack}
\title{Implementation of the brute_force_knapsack function.}
\arguments{
\item{x}{A dataframe containing two vectors with the weights (w) and the values (v).}

\item{W}{The maximum capacity.}
}
\value{
A list containing the value and the elements used to fill the knapsack.
}
\description{
The following algorithm, finds the best combination of the elements in terms of value-weight.
}
\examples{
#' set.seed(42)
n <- 2000
w=sample(1:4000, size = n, replace = TRUE)
v=runif(n = n, 0, 10000)
knapsack_objects <- data.frame(w,v)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
}
\references{
\url{https://en.wikipedia.org/wiki/Knapsack_problem}
}
