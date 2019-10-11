#' Implementation of the knapsack_dynamic function. 
#' @name knapsack_dynamic
#' @param x A dataframe containing two vectors with the weights (w) and the values (v).
#' @param W The maximum capacity.
#' @return A list containing the value and the elements used to fill the knapsack.
#' @description The following algorithm, finds the best combination of the elements in terms of value-weight.
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @import tictoc
#' @export knapsack_dynamic
#' @examples
#' #' set.seed(42)
#' n <- 2000
#' w=sample(1:4000, size = n, replace = TRUE)
#' v=runif(n = n, 0, 10000)
#' knapsack_objects <- data.frame(w,v)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_dynamic <- function(x, W){
  n <- length(x[,1])
  m<-matrix(0, nrow=n,ncol=W)
  for(i in 2:n){
    for(j in 1:W){
      if(x[i,1]>j){
        m[i,j]<-m[i-1,j]
      }else{
        m[i,j]<-max(m[i-1,j], m[i-1,j-x[i,1]] + x[i,2])
      }
    }
  }
  elements <- c()
  temp_counter <- c(length(x[,1]) , W )
  for(i in n:2){
    if(m[i,temp_counter[2]] == m[i-1,temp_counter[2]]) {
      temp_counter[1] <- temp_counter[1] - 1
    }
    else {
      temp_counter[2] <- temp_counter[2]- x[i,1]
      temp_counter[1] <- temp_counter[1] - 1
      elements <- c(elements,i)
    }
  }
  elements<-sort(elements, decreasing = FALSE)
  lst <- list()
  lst$value <- round(m[n,W])
  lst$elements <- elements
  return(lst)
}