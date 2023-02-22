library(tidymodels)
library(tidyverse)
library(tuneRanger)
library(furrr)
library(future)
library(tictoc)

source("fdt.R")
source("fdo.R")
source("sdt.R")
source("sdo.R")
source("sdcv.R")

plan(multisession, workers = 100)

n <- 50
p <- 100
prop <- 0.83
reps <- 1000
strat <- "sdt"

start_time <- Sys.time()
data <- future_map_dfr(1:reps,sdt)
end_time <- Sys.time()

sink(paste0("time_",strat,"_",n,"_",p,"_",prop,".csv"))
print(end_time - start_time)
sink()

write.csv(data, paste0(paste(paste(strat,n,p,prop, sep = "_")),".csv"))
