library(tidymodels)
library(tidyverse)
library(tuneRanger)
library(furrr)
library(future)
library(tictoc)
library(caret)

source("newfdo.R")
source("newsdt.R")
source("newsdo.R")
source("newsdcv.R")
source("newfdcv.R")
source("fdcvlogreg.R")

plan(multisession, workers = 100)

n <- 1120
p <- round(n/5)
prop <- 0.5
reps <- 5000
n.holdout <- 20000
strat <- "fdcvlogreg"

start_time <- Sys.time()
data <- future_map_dfr(1:reps,fdcvlogreg) %>% mutate(err = mean(errxy, na.rm=TRUE))
end_time <- Sys.time()

sink(paste0("time_",strat,"_",n,"_",p,"_",prop,"_",reps,".csv"))
print(end_time - start_time)
sink()

write.csv(data, paste0(paste(paste(strat,n,p,prop,reps, sep = "_")),".csv"))
