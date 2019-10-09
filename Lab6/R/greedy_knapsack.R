library(tictoc)

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

greedy_knapsack <- function(x, W){
  tic()
  if(!is.data.frame(x) || !is.numeric(W) || W<=0){
    stop()
  }
  x$ratio <- x$v/x$w
  sort_by_ratio <- x[order(-x$ratio),]
  tempWeight <- 0
  tempValue <- 0
  temp_id <- c()
  
  for(i in 1:length(sort_by_ratio$ratio)){
    if(tempWeight <= W){
      tempWeight <- tempWeight + sort_by_ratio$w[i]
      tempValue <- tempValue + sort_by_ratio$v[i]
      temp_id <- c(temp_id, row.names(sort_by_ratio[i,]))
    }
  }
  lst <- list()
  lst$value <- round(tempValue)
  lst$elements <- temp_id
  toc()
  return(lst)
}