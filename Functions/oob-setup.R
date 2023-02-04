#Grid search for best oob error
mtry.tune <- function(training.data){
  df <- tuneMtryFast(class~., data = training.data, num.treesTry=500, improve = 1e-5, 
                     plot = FALSE, trace = FALSE, doBest = TRUE)
  return(df)
}
