library(tidymodels)
library(tidyverse)
library(tuneRanger)
library(furrr)
library(future)
library(tictoc)

source("~/fdt.R")
source("~/fdo.R")
source("~/sdt.R")
source("~/sdo.R")
source("~/sdcv.R")

plan(multisession, workers = 12)
