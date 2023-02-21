source("data-generation.R")
source("errxy.R")
source("oob-setup.R")

err.full.oob <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  model <- mtry.tune(data)
  errs <- model[["prediction.error"]]
  dat <- as.data.frame(cbind(data$class, model[["predictions"]]))
  errs_fpr <- sum(dat$V1==1 & dat$V2==2)/(sum(dat$V2==2))
  errs_fnr <- sum(dat$V1==2 & dat$V2==1)/(sum(dat$V1==1))
  return(data.frame(errs, errs_fpr, errs_fnr))
}

err.full.oob2 <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.3))
  model <- mtry.tune(data)
  errs <- model[["prediction.error"]]
  dat <- as.data.frame(cbind(data$class, model[["predictions"]]))
  errs_fpr <- sum(dat$V1==1 & dat$V2==2)/(sum(dat$V2==2))
  errs_fnr <- sum(dat$V1==2 & dat$V2==1)/(sum(dat$V1==1))
  return(data.frame(errs, errs_fpr, errs_fnr))
}

fdo <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  model <- mtry.tune(data)
  err.hat <- model[["prediction.error"]]
  dat <- as.data.frame(model[["confusion.matrix"]])
  fpr.hat <- dat$Freq[2]/(dat$Freq[2]+dat$Freq[4])
  fnr.hat <- dat$Freq[3]/(dat$Freq[3]+dat$Freq[1])
  errxy <- map_dfr(1:100, errxy, model=model) %>% summarise(errxy = mean(errs), errxy_fpr = mean(errs_fpr), errxy_fnr = mean(errs_fnr))
  err <- map_dfr(1:100, err.full.oob) %>% summarise(err = mean(errs), err_fpr = mean(errs_fpr),err_fnr = mean(errs_fnr))
  return(data.frame(err.hat, fpr.hat, fnr.hat, errxy, err, n, p, prop))
}

fdo2 <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.3))
  model <- mtry.tune(data)
  err.hat <- model[["prediction.error"]]
  dat <- as.data.frame(model[["confusion.matrix"]])
  fpr.hat <- dat$Freq[2]/(dat$Freq[2]+dat$Freq[4])
  fnr.hat <- dat$Freq[3]/(dat$Freq[3]+dat$Freq[1])
  errxy <- map_dfr(1:100, errxy, model=model) %>% summarise(errxy = mean(errs), errxy_fpr = mean(errs_fpr), errxy_fnr = mean(errs_fnr))
  err <- map_dfr(1:100, err.full.oob2) %>% summarise(err = mean(errs), err_fpr = mean(errs_fpr),err_fnr = mean(errs_fnr))
  return(data.frame(err.hat, fpr.hat, fnr.hat, errxy, err, n, p, prop))
}
