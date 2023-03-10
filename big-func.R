library(tidymodels)
library(tidyverse)
library(tuneRanger)
library(furrr)
library(future)
library(tictoc)

source("newfdo.R")
source("newsdt.R")
source("newsdo.R")
source("newsdcv.R")
source("newfdcv.R")

plan(multisession, workers = 100)

n <- 250
p <- 10
prop <- 0.5
reps <- 1000
n.holdout <- 20000
strat <- "fdo"

start_time <- Sys.time()
data <- future_map_dfr(1:reps,fdo) %>% mutate(err = mean(errxy), err_fpr = mean(errxy_fpr), err_fnr = mean(errxy_fnr))
end_time <- Sys.time()

sink(paste0("time_",strat,"_",n,"_",p,"_",prop,".csv"))
print(end_time - start_time)
sink()

write.csv(data, paste0(paste(paste(strat,n,p,prop, sep = "_")),".csv"))
