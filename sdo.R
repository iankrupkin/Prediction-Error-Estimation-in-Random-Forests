source("~/data-generation.R")
source("~/errxy.R")
source("~/oob-setup.R")

err.split.oob <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  data.split <- initial_split(data, 0.75, strata = class)
  data.train <- training(data.split)
  data.test <- testing(data.split)
  model <- mtry.tune(data.train)
  data2 <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model, data2)
  dat <- as.data.frame(cbind(data2$class, preds[["predictions"]]))
  errs_fpr <- sum(dat$V1==1 & dat$V2==2)/(sum(dat$V2==2))
  errs_fnr <- sum(dat$V1==2 & dat$V2==1)/(sum(dat$V1==1))
  errs <- sum(preds[["predictions"]] != data2$class)/length(data2$class)
  return(data.frame(errs, errs_fpr,errs_fnr))
}

sdo <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  data.split <- initial_split(data, 0.75, strata = class)
  data.train <- training(data.split)
  data.test <- testing(data.split)
  model <- mtry.tune(data.train)
  preds <- predict(model, data.test)
  err.hat <- sum(preds[["predictions"]] != data.test$class)/length(data.test$class)
  dat <- as.data.frame(cbind(data.test$class, preds[["predictions"]]))
  fpr.hat <- sum(dat$V1==1 & dat$V2==2)/(sum(dat$V2==2))
  fnr.hat <- sum(dat$V1==2 & dat$V2==1)/(sum(dat$V1==1))
  errxy <- map_dfr(1:100, errxy, model=model) %>% summarise(errxy = mean(errs), errxy_fpr = mean(errs_fpr), errxy_fnr = mean(errs_fnr))
  err <- map_dfr(1:100, err.split.oob) %>% summarise(err = mean(errs), err_fpr = mean(errs_fpr),err_fnr = mean(errs_fnr))
  return(data.frame(err.hat, fpr.hat, fnr.hat, errxy, err, n, p, prop))
}
