set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

brute_force_knapsack <- function(x, W){
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
  return(lst)
}