source("data-generation.R")
source("oob-setup.R")

fdcvlogreg <- function(x){
  data <- data.frame(matrix(rnorm(n * p), nrow = n),class=as.factor(c(rep(1,prop*n),rep(2,(1-prop)*n))))
  ho.data <- data.frame(matrix(rnorm(n.holdout * p), nrow = n.holdout),class=as.factor(c(rep(1,prop*n.holdout),rep(2,(1-prop)*n.holdout))))
  train.control <- trainControl(method = "cv", number = 4)
  model <- train(class~., data = data, method = "glm", family=binomial, trControl = train.control)
  err.hat <- 1-model[["results"]][["Accuracy"]]
  preds <- predict(model, ho.data)
  dat2 <- as.data.frame(cbind(ho.data$class, preds))
  errxy <- sum(dat2$V1 != dat2$preds)/length(dat2$V1)
  return(data.frame(err.hat, errxy, n, p, prop))
}
