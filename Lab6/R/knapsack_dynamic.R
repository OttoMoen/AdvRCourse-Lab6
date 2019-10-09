library(tictoc)

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

knapsack_dynamic <- function(x, W){
  tic()
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
  toc()
  return(lst)
}