source("~/Functions/data-generation.R")
source("~/Functions/errxy.R")
source("~/Functions/oob-setup.R")

err.full.oob <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  model <- mtry.tune(data)
  data2 <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model, data2)
  errs <- sum(preds[["predictions"]] != data2$class)/length(data2$class)
  return(data.frame(errs))
}

oob.est.full <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  model <- mtry.tune(data)
  oob.err.est <- model[["prediction.error"]]
  errxy <- map_dfr(1:100, errxy, model=model) %>% summarise(errxy = mean(errs))
  err <- map_dfr(1:100, err.full.oob) %>% summarise(err = mean(errs))
  return(data.frame(oob.err.est, errxy, err, n, p, prop))
}
