library(rstudioapi)
library(data.table)
library(tidyverse)
library(AdhereR)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# create simulated data for settings with:
#
# - mean number of drugs per patient: 5, 8
# - meadian duration of generated dispensations: 60, 75, 90, 105
# source("1_create_simulated_data.R")

source("timeslicer.R")

# load simulated data
source("2_load_simulated_data.R")

# compute sensitivity and specificity for each setting
sim_data <- sim_data_8_105 # change according to the one of interest (it would take a very long time to run the entire code)
source("3_compute_accuracy.R")

# see sensitivity
t[1] # rows: dispensation length in analysis (60, 90, 120); # columns: number of dispensations (2,3,4)

# see specificity
t[2]
