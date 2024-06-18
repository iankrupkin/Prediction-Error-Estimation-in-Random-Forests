library(tidymodels)
library(tidyverse)
library(tuneRanger)
library(furrr)
library(future)
library(tictoc)
library(caret)

source("fdo.R")
source("sdt.R")
source("sdo.R")
source("sdcv.R")
source("fdcv.R")
source("lgcv.R")

plan(multisession, workers = 50)

n <- 50
p <- 10
prop <- 0.5
reps <- 1000
n.holdout <- 20000
mu2 <- 0.2
strat <- "fdcv"

start_time <- Sys.time()
data <- future_map_dfr(1:reps,fdcv) %>% mutate(err = mean(errxy, na.rm=TRUE))
end_time <- Sys.time()

sink(paste0("time_",strat,"_",n,"_",p,"_",prop,"_",mu2,"_",reps,".csv"))
print(end_time - start_time)
sink()

write.csv(data, paste0(paste(paste(strat,n,p,prop,mu2,reps, sep = "_")),".csv"))
