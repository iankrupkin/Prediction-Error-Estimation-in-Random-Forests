source("~/Functions/data-generation.R")
source("~/Functions/errxy.R")
source("~/Functions/oob-setup.R")

err.cv.split <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  data.split <- initial_split(data, 0.75, strata = class)
  data.train <- training(data.split)
  data.test <- testing(data.split)
  recipe <- recipe(class~., data = data.train, strata=class)
  model <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>% set_engine("randomForest") %>% set_mode("classification")
  workflow <- workflow() %>% add_model(model) %>% add_recipe(recipe)
  folds <- vfold_cv(data.train, v=4)
  grid <- grid_regular(mtry(range=c(1,p)), levels = 10)
  tune <- workflow %>% tune_grid(resamples = folds, grid = grid)
  model.best <- finalize_model(model, select_best(tune, "accuracy"))
  model.best.fit <- workflow() %>% add_model(model.best) %>% add_recipe(recipe) %>% 
    fit(data = data.train)
  data2 <- data.frame(pmap(list(n,p,prop),power.data.2))
  preds <- predict(model.best.fit, data2)
  errs <- sum(preds$.pred_class != data2$class)/length(data2$class)
  return(data.frame(errs))
}

sdcv <- function(x){
  data <- data.frame(pmap(list(n,p,prop),power.data.2))
  data.split <- initial_split(data, 0.75, strata = class)
  data.train <- training(data.split)
  data.test <- testing(data.split)
  recipe <- recipe(class~., data = data.train, strata=class)
  model <- rand_forest(mtry = tune(), trees = 500, min_n = 5) %>% set_engine("randomForest") %>% set_mode("classification")
  workflow <- workflow() %>% add_model(model) %>% add_recipe(recipe)
  folds <- vfold_cv(data.train, v=4)
  grid <- grid_regular(mtry(range=c(1,p)), levels = 10)
  tune <- workflow %>% tune_grid(resamples = folds, grid = grid)
  model.best <- finalize_model(model, select_best(tune, "accuracy"))
  model.best.fit <- workflow() %>% add_model(model.best) %>% add_recipe(recipe) %>% 
    fit(data = data.train)
  df <- model.best.fit %>% 
    predict(new_data = data.test) %>%
    cbind(data.test)
  error.est = sum(df$.pred_class != df$class)/length(df$class)
  errxy <- map_dfr(1:1000, errxy2, model=model.best.fit) %>% summarise(errxy = mean(errs))
  err <- future_map_dfr(1:100, err.cv.split) %>% summarise(err = mean(errs))
  return(data.frame(error.est,errxy,err, n, p, prop))
}
