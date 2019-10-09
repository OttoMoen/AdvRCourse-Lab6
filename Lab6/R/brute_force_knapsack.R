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

knapsack_dynamic <- function(x, W){
  n <- length(x[,1])
  m<-matrix(0, nrow=n,ncol=W)
  for(i in 2:n){
    for(j in 1:W){
      #print(x[j,1])
      #print(j)
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