source("data-generation.R")
source("errxy.R")
source("oob-setup.R")

sdt <- function(x){
  data.train <- data.frame(pmap(list(round(0.75*0.25*n)+1,p,prop),power.data.2))
  data.validation <- data.frame(pmap(list(round(0.75*0.75*n)+1,p,prop),power.data.2))
  data.test <- data.frame(pmap(list(0.25*n,p,prop),power.data.2))
  ho.data <- data.frame(pmap(list(n.holdout,p,prop),power.data.2))
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
  err.hat = sum(df$.pred_class != df$class)/length(df$class)
  fpr.hat <- sum(df$class==1 & df$.pred_class==2)/(sum(df$.pred_class==2))
  fnr.hat <- sum(df$class==2 & df$.pred_class==1)/(sum(df$class==1))
  preds <- predict(model.best.fit,ho.data)
  dat2 <- as.data.frame(cbind(ho.data$class, preds$.pred_class))
  errxy <- sum(dat2$V1 != dat2$V2)/length(dat2$V2)
  errxy_fpr <- sum(dat2$V1==1 & dat2$V2==2)/(sum(dat2$V2==2))
  errxy_fnr <- sum(dat2$V1==2 & dat2$V2==1)/(sum(dat2$V1==1))
  return(data.frame(err.hat, fpr.hat, fnr.hat, errxy, errxy_fpr, errxy_fnr, n, p, prop))
}
