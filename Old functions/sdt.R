source("data-generation.R")
source("errxy.R")
source("oob-setup.R")

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
  df <- model.best.fit %>% 
    predict(new_data = data.test) %>%
    cbind(data.test)
  errs <- sum(df$.pred_class != df$class)/length(df$class)
  errs_fpr <- sum(df$class==1 & df$.pred_class==2)/(sum(df$.pred_class==2))
  errs_fnr <- sum(df$class==2 & df$.pred_class==1)/(sum(df$class==1))
  return(data.frame(errs, errs_fpr,errs_fnr))
}

err.split.acc2 <- function(x){
  data.train <- data.frame(pmap(list(round(0.75*0.25*n),p,prop),power.data.3))
  data.validation <- data.frame(pmap(list(round(0.75*0.75*n)+1,p,prop),power.data.3))
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
  df <- model.best.fit %>% 
    predict(new_data = data.test) %>%
    cbind(data.test)
  errs <- sum(df$.pred_class != df$class)/length(df$class)
  errs_fpr <- sum(df$class==1 & df$.pred_class==2)/(sum(df$.pred_class==2))
  errs_fnr <- sum(df$class==2 & df$.pred_class==1)/(sum(df$class==1))
  return(data.frame(errs, errs_fpr,errs_fnr))
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
  err.hat = sum(df$.pred_class != df$class)/length(df$class)
  fpr.hat <- sum(df$class==1 & df$.pred_class==2)/(sum(df$.pred_class==2))
  fnr.hat <- sum(df$class==2 & df$.pred_class==1)/(sum(df$class==1))
  errxy <- map_dfr(1:100, errxy2, model=model.best.fit) %>% summarise(errxy = mean(errs), errxy_fpr = mean(errs_fpr), errxy_fnr = mean(errs_fnr))
  err <- map_dfr(1:100, err.split.acc) %>% summarise(err = mean(errs), err_fpr = mean(errs_fpr),err_fnr = mean(errs_fnr))
  return(data.frame(err.hat, fpr.hat, fnr.hat, errxy, err, n, p, prop))
}

sdt2 <- function(x){
  data.train <- data.frame(pmap(list(round(0.75*0.25*n),p,prop),power.data.3))
  data.validation <- data.frame(pmap(list(round(0.75*0.75*n)+1,p,prop),power.data.3))
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
  err.hat = sum(df$.pred_class != df$class)/length(df$class)
  fpr.hat <- sum(df$class==1 & df$.pred_class==2)/(sum(df$.pred_class==2))
  fnr.hat <- sum(df$class==2 & df$.pred_class==1)/(sum(df$class==1))
  errxy <- map_dfr(1:100, errxy2, model=model.best.fit) %>% summarise(errxy = mean(errs), errxy_fpr = mean(errs_fpr), errxy_fnr = mean(errs_fnr))
  err <- map_dfr(1:100, err.split.acc2) %>% summarise(err = mean(errs), err_fpr = mean(errs_fpr),err_fnr = mean(errs_fnr))
  return(data.frame(err.hat, fpr.hat, fnr.hat, errxy, err, n, p, prop))
}
