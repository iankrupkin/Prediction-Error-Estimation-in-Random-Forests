library(tidymodels)
library(tidyverse)
library(tuneRanger)
library(furrr)
library(future)
library(tictoc)
library(caret)
library(randomForest)

source("fdo.R")
source("sdt.R")
source("sdo.R")
source("sdcv.R")
source("fdcv.R")
source("fdcv_noisy.R")
source("lgcv.R")

plan(multisession, workers = 10)#100)

n <- c(10,25,50,75,100,250,500,750,1000,25,50,75,100,250,500,750,1000)
p <- c(rep(10,9),rep(100,8))
prop <- rep(0.5,17)
mu2 <- rep(0.75, 17)

rep_numb <- 1:100#1000
n.holdout <- 20000
strat <- "fdcv"

err_input <- tidyr::expand_grid(data.frame(n=n,p=p,prop=prop,mu2=mu2), rep_numb)

start_time <- Sys.time()
data.75.noisy <- err_input |> 
        furrr::future_pmap(fdcv_noisy)  |> 
        list_rbind() |> 
        mutate(err = mean(errxy, na.rm=TRUE))
end_time <- Sys.time()

#sink(paste0("time_",strat,"_",0.2,".csv"))
#print(end_time - start_time)
#sink()

write_csv(data.75.noisy, paste0(paste(paste(strat,0.75,"noisy", sep = "_")),".csv"))

