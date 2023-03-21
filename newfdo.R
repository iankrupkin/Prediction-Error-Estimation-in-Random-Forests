source("data-generation.R")
source("oob-setup.R")

fdo <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  ho.data <- data.frame(pmap(list(n.holdout,p,prop),power.data.2))
  model <- mtry.tune(data)
  err.hat <- model[["prediction.error"]]
  dat <- as.data.frame(model[["confusion.matrix"]])
  preds <- predict(model,ho.data)
  dat2 <- as.data.frame(cbind(ho.data$class, preds[["predictions"]]))
  errxy <- sum(dat2$V1 != dat2$V2)/length(dat2$V2)
  return(data.frame(err.hat, errxy, n, p, prop))
}
