#' Implementation of the brute_force_knapsack function. 
#' @name brute_force_knapsack
#' @param x A dataframe containing two vectors with the weights (w) and the values (v).
#' @param W The maximum capacity.
#' @return A list containing the value and the elements used to fill the knapsack.
#' @description The following algorithm, finds the best combination of the elements in terms of value-weight.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @import tictoc
#' @export brute_force_knapsack
#' @examples
#' #' set.seed(42)
#' n <- 2000
#' w=sample(1:4000, size = n, replace = TRUE)
#' v=runif(n = n, 0, 10000)
#' knapsack_objects <- data.frame(w,v)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

library(tictoc)

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

brute_force_knapsack <- function(x, W){
  tic()
  if(!is.data.frame(x) || !is.numeric(W) || W<=0){
    stop()
  }
  maxValue <- 0
  best_id <- c()
  n <- length(x[,1])
  for(i in 1:(2^n)-1){
    tempWeight <- 0
    tempValue <- 0
    temp_id <- c()
    bits <- intToBits(i)
    for(j in 1:length(bits)){
      if(bits[j] == TRUE){
        tempWeight <- tempWeight + x[j,1]
        tempValue <- tempValue + x[j,2]
        temp_id <- c(temp_id,j)
      }
    }
    if(tempValue > maxValue && tempWeight <= W){
      maxValue <- tempValue
      best_id <- temp_id
    }
  }
  lst <- list()
  lst$value <- round(maxValue)
  lst$elements <- best_id
  toc()
  return(lst)
}