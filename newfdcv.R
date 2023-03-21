source("data-generation.R")
source("oob-setup.R")

fdcv <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  ho.data <- data.frame(pmap(list(n.holdout,p,prop),power.data.2))
  recipe <- recipe(class~., data = data, strata=class)
  model <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>% set_engine("randomForest") %>% set_mode("classification")
  workflow <- workflow() %>% add_model(model) %>% add_recipe(recipe)
  folds <- vfold_cv(data, v=4)
  grid <- grid_regular(mtry(range=c(1,p)), levels = 10)
  tune <- workflow %>% tune_grid(resamples = folds, grid = grid)
  model.best <- finalize_model(model, select_best(tune, "accuracy"))
  model.best.fit <- workflow() %>% add_model(model.best) %>% add_recipe(recipe) %>% 
    fit(data = data)
  df <- data.frame(model.best.fit[["fit"]][["fit"]][["fit"]][["confusion"]])
  err.hat <- (df$X1[2]+df$X2[1])/(df$X1[1]+df$X2[2]+df$X1[2]+df$X2[1])
  fpr.hat <- df$X2[1]/(df$X2[1]+df$X2[2])
  fnr.hat <- df$X1[2]/(df$X1[2]+df$X1[1])
  preds <- predict(model.best.fit,ho.data)
  dat2 <- as.data.frame(cbind(ho.data$class, preds$.pred_class))
  errxy <- sum(dat2$V1 != dat2$V2)/length(dat2$V2)
  errxy_fpr <- sum(dat2$V1==1 & dat2$V2==2)/(sum(dat2$V2==2))
  errxy_fnr <- sum(dat2$V1==2 & dat2$V2==1)/(sum(dat2$V1==1))
  return(data.frame(err.hat, fpr.hat, fnr.hat, errxy, errxy_fpr, errxy_fnr, n, p, prop))
}

fdcv2 <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  ho.data <- data.frame(pmap(list(n.holdout,p,prop),power.data.2))
  train.control <- trainControl(method = "cv", number = 4)
  model <- train(class~., data = data, method = "rf", tuneGrid=data.frame(mtry=sqrt(p)), trees = 500, min_n = 5, trControl = train.control)
  err.hat <- 1-model[["results"]][["Accuracy"]]
  preds <- predict(model, ho.data)
  dat2 <- as.data.frame(cbind(ho.data$class, preds))
  errxy <- sum(dat2$V1 != dat2$preds)/length(dat2$V1)
  return(data.frame(err.hat, errxy, n, p, prop))
}
