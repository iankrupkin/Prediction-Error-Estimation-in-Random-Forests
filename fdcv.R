source("data-generation.R")
source("oob-setup.R")

fdcv <- function(x){
  data <- data.frame(pmap(list(n,p,prop),data.generation))
  ho.data <- data.frame(pmap(list(n.holdout,p,prop),data.generation))
  train.control <- trainControl(method = "cv", number = 4)
  model <- train(class~., data = data, method = "rf", tuneGrid=data.frame(mtry=sqrt(p)), trees = 500, min_n = 5, trControl = train.control)
  err.hat <- 1-model[["results"]][["Accuracy"]]
  preds <- predict(model, ho.data)
  dat2 <- as.data.frame(cbind(ho.data$class, preds))
  errxy <- sum(dat2$V1 != dat2$preds)/length(dat2$V1)
  return(data.frame(err.hat, errxy, n, p, prop))
}
