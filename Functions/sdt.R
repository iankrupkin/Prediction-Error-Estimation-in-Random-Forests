source("~/Functions/data-generation.R")
source("~/Functions/errxy.R")
source("~/Functions/oob-setup.R")

err.split.acc <- function(x){
  data.train <- data.frame(pmap(list(round(0.75*0.25*n)+1,p,prop),power.data.2))
  data.validation <- data.frame(pmap(list(round(0.75*0.75*n)+1,p,prop),power.data.2))
  data.test <- data.frame(pmap(list(0.25*n,p,prop),power.data.2))
  recipe <- recipe(class~., data = data.train, strata=class)
  model <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>% set_engine("randomForest") %>% set_mode("classification")
  workflow <- workflow() %>% add_model(model) %>% add_recipe(recipe)
  resamples <- apparent(data.validation)
  grid <- grid_regular(mtry(range=c(1,p)), levels = 10)
  tune <- workflow %>% tune_grid(resamples = resamples, grid = grid)
  model.best <- finalize_model(model, select_best(tune, "accuracy"))
  model.best.fit <- workflow() %>% add_model(model.best) %>% add_recipe(recipe) %>% 
    fit(data = data.validation)
  data2 <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model.best.fit, data2)
  errs <- sum(preds$.pred_class != data2$class)/length(data2$class)
  return(data.frame(errs))
}

sdt <- function(x){
  data.train <- data.frame(pmap(list(round(0.75*0.25*n)+1,p,prop),power.data.2))
  data.validation <- data.frame(pmap(list(round(0.75*0.75*n)+1,p,prop),power.data.2))
  data.test <- data.frame(pmap(list(0.25*n,p,prop),power.data.2))
  recipe <- recipe(class~., data = data.train, strata=class)
  model <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>% set_engine("randomForest") %>% set_mode("classification")
  workflow <- workflow() %>% add_model(model) %>% add_recipe(recipe)
  resamples <- apparent(data.validation)
  grid <- grid_regular(mtry(range=c(1,p)), levels = 10)
  tune <- workflow %>% tune_grid(resamples = resamples, grid = grid)
  best <- tune[[3]][[1]] %>% filter(.metric == "accuracy") %>% filter(.metric == min(.metric)) %>% head(1) 
  model.best <- finalize_model(model, select_best(tune, "accuracy"))
  model.best.fit <- workflow() %>% add_model(model.best) %>% add_recipe(recipe) %>% 
    fit(data = data.validation)
  df <- model.best.fit %>% 
    predict(new_data = data.test) %>%
    cbind(data.test)
  error.est = sum(df$.pred_class != df$class)/length(df$class)
  errxy <- map_dfr(1:1000, errxy2, model=model.best.fit) %>% summarise(errxy = mean(errs))
  err <- future_map_dfr(1:100, err.split.acc) %>% summarise(err = mean(errs))
  return(data.frame(error.est, errxy, err, n, p, prop))
}
