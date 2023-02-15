power.data.2 <- function(n,p,prop) {
  data <- data.frame()
  class <- as.factor(c(rep(1,prop*n),rep(2,(1-prop)*n)))
  data <- data.frame(class)
  if(p == 10){
    variables1 <- 1:2
    variables2 <- 3:10
  }else if(p == 100){
    variables1 <- 1:10
    variables2 <- 11:100
  } else{
    variables1 <- 1:50
    variables2 <- 51:1000}
  data[paste0('X', variables1)] <- map_dfc(variables1, 
                                           ~c(rnorm(prop*n,0,1),rnorm((1-prop)*n,0.75,1)))
  data[paste0('X', variables2)] <- map_dfc(variables2, ~rnorm(n,0,1))
  return(data)
}

null.data.2 <- function(n,p,prop) {
  class <- as.factor(c(rep(1,prop*n),rep(2,(1-prop)*n)))
  data <- data.frame(class)
  variables <- 1:p
  data[paste0('X', variables)] <- map_dfc(variables, ~rnorm(n,0,1))
  return(data)
}