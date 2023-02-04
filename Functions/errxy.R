errxy <- function(x,model){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model, data)
  errs <- sum(preds[["predictions"]] != data$class)/length(data$class)
  return(data.frame(errs))
}

errxy2 <- function(x, model){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model, data)
  errs <- sum(preds$.pred_class != data$class)/length(data$class)
  return(data.frame(errs))
}