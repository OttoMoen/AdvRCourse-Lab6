set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

brute_force_knapsack <- function(x, W){
  N<-length(x$w)
  tempWeight<-0
  tempValue<-0
  minWeight<-99999999
  maxValue<-0
  best_i<-c()
  
  for(i in 1:N){
    temp_i<-c()
    tempWeight<-tempWeight + x$w[i]
    tempValue<- tempWeight + x$v[i]
    temp_i<-c(temp_i, i)
    
    if(tempWeight <= W && tempValue >= maxValue){
      best_i<-temp_i
      if(tempValue > maxValue){
        maxValue <- tempValue

      }
    }
  }
  return(best_i)
}