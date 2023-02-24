source("data-generation.R")
source("oob-setup.R")

fdo <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  ho.data <- data.frame(pmap(list(n.holdout,p,prop),power.data.2))
  model <- mtry.tune(data)
  err.hat <- model[["prediction.error"]]
  dat <- as.data.frame(model[["confusion.matrix"]])
  fpr.hat <- dat$Freq[2]/(dat$Freq[2]+dat$Freq[4])
  fnr.hat <- dat$Freq[3]/(dat$Freq[3]+dat$Freq[1])
  preds <- predict(model,ho.data)
  dat2 <- as.data.frame(cbind(ho.data$class, preds[["predictions"]]))
  errxy <- sum(dat2$V1 != dat2$V2)/length(dat2$V2)
  errxy_fpr <- sum(dat2$V1==1 & dat2$V2==2)/(sum(dat2$V2==2))
  errxy_fnr <- sum(dat2$V1==2 & dat2$V2==1)/(sum(dat2$V1==1))
  return(data.frame(err.hat, fpr.hat, fnr.hat, errxy, errxy_fpr, errxy_fnr, n, p, prop))
}
