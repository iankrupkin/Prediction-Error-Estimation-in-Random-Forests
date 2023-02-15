source("data-generation.R")
source("errxy.R")
source("oob-setup.R")

err.full.acc <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  recipe <- recipe(class~., data = data, strata=class)
  model <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>% set_engine("randomForest") %>% set_mode("classification")
  workflow <- workflow() %>% add_model(model) %>% add_recipe(recipe)
  resamples <- apparent(data)
  grid <- grid_regular(mtry(range=c(1,p)), levels = 10)
  tune <- workflow %>% tune_grid(resamples = resamples, grid = grid)
  model.best <- finalize_model(model, select_best(tune, "accuracy"))
  model.best.fit <- workflow() %>% add_model(model.best) %>% add_recipe(recipe) %>% 
    fit(data = data)
  data2 <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model.best.fit, data2)
  errs <- sum(preds$.pred_class != data2$class)/length(data2$class)
  return(data.frame(errs))
}

fdt <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  recipe <- recipe(class~., data = data, strata=class)
  model <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>% set_engine("randomForest") %>% set_mode("classification")
  workflow <- workflow() %>% add_model(model) %>% add_recipe(recipe)
  resamples <- apparent(data)
  grid <- grid_regular(mtry(range=c(1,p)), levels = 10)
  tune <- workflow %>% tune_grid(resamples = resamples, grid = grid)
  best <- tune[[3]][[1]] %>% filter(.metric == "accuracy") %>% filter(.metric == min(.metric)) %>% head(1) 
  error.est <- 1-best$.estimate
  model.best <- finalize_model(model, select_best(tune, "accuracy"))
  model.best.fit <- workflow() %>% add_model(model.best) %>% add_recipe(recipe) %>% 
    fit(data = data)
  errxy <- map_dfr(1:1000, errxy2, model=model.best.fit) %>% summarise(errxy = mean(errs))
  err <- future_map_dfr(1:100, err.full.acc) %>% summarise(err = mean(errs))
  return(data.frame(error.est, errxy, err, n, p, prop))
}
