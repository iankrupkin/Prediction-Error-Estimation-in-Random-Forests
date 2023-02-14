library(tidymodels)
library(tidyverse)
library(tuneRanger)
library(furrr)
library(future)
library(tictoc)

source("~/Functions/fdt.R")
source("~/Functions/fdo.R")
source("~/Functions/sdt.R")
source("~/Functions/sdo.R")
source("~/Functions/sdcv.R")

plan(multisession, workers = 50)

n <- 50
p <- 10
prop <- 0.5
reps <- 1
strat <- "fdo"

start_time <- Sys.time()
data <- future_map_dfr(1:reps,fdo)
end_time <- Sys.time()

sink(paste0("time_",strat,"_",n,"_",p,"_",prop,".csv"))
print(end_time - start_time)
sink()

write.csv(data, paste0(paste(paste(strat,n,p,prop, sep = "_")),".csv"))
