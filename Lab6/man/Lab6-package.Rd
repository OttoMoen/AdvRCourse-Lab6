\name{Lab6-package}
\alias{Lab6-package}
\alias{Lab6}
\docType{package}
\title{
\packageTitle{Lab6}
}
\description{
\packageDescription{Lab6}
}
\details{
Implementation of the knapsack algorithm in three different ways: Brute Force, Greedy and Dynamic method.

The DESCRIPTION file:
\packageDESCRIPTION{Lab6}	
\packageIndices{Lab6}	
~~ An overview of how to use the package, including the most important functions ~~	
}	
\author{	
\packageAuthor{Lab6}	

Maintainer: \packageMaintainer{Lab6}	
}	
\references{	
[1] \url{https://en.wikipedia.org/wiki/Knapsack_problem}

[2] Hristakeva, M., & Shrestha, D. (2005, April). Different approaches to solve the 0/1 knapsack problem. In The Midwest Instruction and Computing Symposium. 
}	

\examples{	
 set.seed(42)
 n <- 2000
 w=sample(1:4000, size = n, replace = TRUE)
 v=runif(n = n, 0, 10000)
 knapsack_objects <- data.frame(w,v)
 
 brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
 greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
 knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
}