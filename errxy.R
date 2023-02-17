errxy <- function(x,model){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model, data)
  dat <- as.data.frame(cbind(data$class, preds[["predictions"]]))
  errs_fpr <- sum(dat$V1==1 & dat$V2==2)/(sum(dat$V2==2))
  errs_fnr <- sum(dat$V1==2 & dat$V2==1)/(sum(dat$V1==1))
  errs <- sum(preds[["predictions"]] != data$class)/length(data$class)
  return(data.frame(errs, errs_fpr,errs_fnr))
}

errxy2 <- function(x, model){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model, data)
  errs <- sum(preds$.pred_class != data$class)/length(data$class)
  dat <- as.data.frame(cbind(data$class, preds))
  errs_fpr <- sum(dat$`data$class`==1 & dat$.pred_class==2)/(sum(dat$.pred_class==2))
  errs_fnr <- sum(dat$`data$class`==2 & dat$.pred_class==1)/(sum(dat$`data$class`==1))
  return(data.frame(errs, errs_fpr, errs_fnr))
}