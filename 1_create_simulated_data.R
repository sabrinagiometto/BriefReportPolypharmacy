library(rstudioapi)
library(tidyverse)
library(data.table)

# Settings ----
#
# - change mean number of drugs (5,8)
# - change median gap time between subsequent dispensations (60, 75, 90, 105 days)

n_drugs <- c(5,8)
duration_generated_Rx_lengths <- c(60, 75, 90, 105)


for (l in n_drugs) {
  
  # generate the number of drugs
  d <- rpois(10000, l)
  
  assign(paste("drug_distrib", l, sep = "_"), d)
  
  # generate medication dispensations with gap times
  vec <- LETTERS[1:max(d)]
  
  set.seed(84)
  
  for (j in duration_generated_Rx_lengths) {
  
    for (k in 1:max(d)) {
      
      ## generate a random variable S from Normal(mu+sigma^2, sigma), set V = e^S ----
      s <- qnorm(runif(10000))*0.5 + (log(j)+(0.5^2))
      V <- exp(s)
      u <- runif(10000, 0, V)
      y <- u
      
      results <- vector()
      tmp <- y
      i <- 1
      time <- 0
      
      while(min(time) <= 365) {
        
        x <- exp(qnorm(runif(10000))*0.5 + log(j))
        tmp <- data.frame(cbind(tmp, x))
        time <- rowSums(tmp[1:i])
        results <- data.frame(cbind(results, time))
        
        i <- i + 1
        
      }
      
      {
        
        # remove time values larger than 365, reshape, 
        results <- results %>%
          mutate(across(everything(.), ~ ifelse(. >= 365, NA, .))) %>% 
          mutate(id = sample(1:dim(results)[1], 10000, replace = F)) %>%
          pivot_longer(cols = -c(id), names_to = "count", values_to = "disp_time") %>% 
          drop_na() %>% 
          select(!count)
        # add drug name
        results <- results %>% mutate(drug = vec[k])
        
        # assign a name 
        assign(paste("disp_time", vec[k], l, j, sep = "_"), results)
        
      }
      
    }
    
  }
  
}

##############

for (j in n_drugs) {
  
  d <- get(paste("drug_distrib", j, sep = "_"))
  
  drugs <- data.frame(id = sample(1:length(d), length(d), replace = F), number_drugs = d)
  
  
  for (l in duration_generated_Rx_lengths) {
      
      list_data <- mget(paste("disp_time", LETTERS[1:max(d)], j, l, sep = "_"))
      
      final <- data.frame(id = numeric(0), disp_time = numeric(0), drug = character(0))
      
      drugs <- drugs %>% arrange(id)
      
      for (i in 1:dim(drugs)[1]) {
        
        z <- drugs[i,"number_drugs"]
        # id_i <- drugs[i,"id"]
        s <- sample(1:max(d), z, replace = F)
        list_i <- lapply(list_data[s], function(x) filter(x, id==i))
        rows_b <- bind_rows(list_i)
        final <- rbind(final, rows_b)
        
      }
      
      final <- final %>% 
        rename(exp_start = disp_time) %>% 
        mutate(obs_start = 0,
               obs_end = 365)
      
      assign(paste("sim_data", j, l, sep = "_"), final)
      
  }
  
}


setwd(dirname(rstudioapi::getSourceEditorContext()$path))

for (i in n_drugs) {
  
  for (j in duration_generated_Rx_lengths) {
    
    df <- get(paste("sim_data", i, j, sep = "_"))
    
    save(df, file = paste0("data\\sim_data_", i,"_", j, ".rda"))
    
  }
}

# save(sim_data, file = "G:\\Il mio Drive\\Dottorato unipi\\SDU\\Brief report polypharmacy PDS\\sim_data.rda")
