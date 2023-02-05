source("~/Functions/data-generation.R")
source("~/Functions/errxy.R")
source("~/Functions/oob-setup.R")

err.split.oob <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  data.split <- initial_split(data, 0.75, strata = class)
  data.train <- training(data.split)
  data.test <- testing(data.split)
  model <- mtry.tune(data.train)
  data2 <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model, data2)
  errs <- sum(preds[["predictions"]] != data2$class)/length(data2$class)
  return(data.frame(errs))
}

sdo <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  data.split <- initial_split(data, 0.75, strata = class)
  data.train <- training(data.split)
  data.test <- testing(data.split)
  model <- mtry.tune(data.train)
  preds <- predict(model, data.test)
  oob.err.est <- sum(preds[["predictions"]] != data.test$class)/length(data.test$class)
  errxy <- map_dfr(1:1000, errxy, model = model) %>% summarise(errxy = mean(errs))
  err <- future_map_dfr(1:100, err.split.oob) %>% summarise(err = mean(errs))
  return(data.frame(oob.err.est, errxy, err, n, p, prop))
}
