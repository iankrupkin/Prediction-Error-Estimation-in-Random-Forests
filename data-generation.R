data.generation <- function(n,p,prop,mu2) {
  data <- data.frame()
  class <- as.factor(c(rep(1,prop*n),rep(2,(1-prop)*n)))
  data <- data.frame(class)
  variables <- 1:p
  data[paste0('X', variables)] <- suppressMessages(map_dfc(variables, 
                                                           ~c(rnorm(prop*n,0,1),rnorm((1-prop)*n,mu2,1))))
  return(data)
}

data.generation.noisy <- function(n,p,prop,mu2) {
  data <- data.frame()
  class <- as.factor(c(rep(1,ceiling(prop*n)),rep(2,floor((1-prop)*n))))
  data <- data.frame(class)
  if(p == 10){
    variables1 <- 1:2
    variables2 <- 3:10
  }else if(p == 100){
    variables1 <- 1:10
    variables2 <- 11:100}
  data[paste0('X', variables1)] <- suppressMessages(map_dfc(variables1, 
                                           ~c(rnorm(ceiling(prop*n),0,1),rnorm(floor((1-prop)*n),mu2,1))))
  data[paste0('X', variables2)] <- suppressMessages(map_dfc(variables2, ~rnorm(n,0,1)))
  return(data)
}

bates.data.generation.noisy <- function(n,p,prop) {
  data <- data.frame()
  class <- as.factor(c(rep(1,prop*n),rep(2,(1-prop)*n)))
  data <- data.frame(class)
  variables1 <- 1:round(p/5)
  variables2 <- round(p/5+1):p
  data[paste0('X', variables1)] <- suppressMessages(map_dfc(variables1, 
                                                            ~c(rnorm(prop*n,0,1),rnorm((1-prop)*n,mu2,1))))
  data[paste0('X', variables2)] <- suppressMessages(map_dfc(variables2, ~rnorm(n,0,1)))
  return(data)
}