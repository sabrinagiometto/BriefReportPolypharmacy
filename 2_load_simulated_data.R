# load simulated data
# set wd
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load simulated data
n_drugs <- c(5,8)
duration_generated_Rx_lengths <- c(60, 75, 90, 105)

for (i in n_drugs) {
  
  for (j in duration_generated_Rx_lengths) {
    
    load(file = paste0("data\\sim_data_", i,"_", j, ".rda"))
    assign(paste0("sim_data_", i,"_", j), df)
    
  }
}

rm(df)