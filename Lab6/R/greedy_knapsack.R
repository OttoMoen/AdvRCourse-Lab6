#' Implementation of the greedy_knapsack function. 
#' @name greedy_knapsack
#' @param x A dataframe containing two vectors with the weights (w) and the values (v).
#' @param W The maximum capacity.
#' @return A list containing the value and the elements used to fill the knapsack.
#' @description The following algorithm, finds the best combination of the elements in terms of value-weight.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @import tictoc
#' @export greedy_knapsack
#' @examples
#' set.seed(42)
#' n <- 2000
#' w=sample(1:4000, size = n, replace = TRUE)
#' v=runif(n = n, 0, 10000)
#' knapsack_objects <- data.frame(w,v)
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

#library(tictoc)

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

greedy_knapsack <- function(x, W){
  #tic()
  if(!is.data.frame(x) || !is.numeric(W) || W<=0){
    stop()
  }
  x$ratio <- x$v/x$w
  x <- x[order(-x$ratio),]
  weight <- 0
  value <- 0
  id <- c()
  
  for(i in 1:length(x$ratio)){
    
    if(weight + x$w[i] <= W){
      print(x$w[i])
      print(weight)
      weight <- weight + x$w[i]
      value <- value + x$v[i]
      id <- c(id, as.numeric(row.names(x[i,])))
      
    }
  }
  lst <- list()
  lst$value <- value
  lst$elements <- id
  #toc()
  return(lst)
}