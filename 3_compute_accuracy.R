
rx_length_in_analysis <- c(60, 90, 120)
required_n_disp_per_drug <- c(2, 3, 4)

sensitivity <- matrix(nrow = length(required_n_disp_per_drug), ncol = length(rx_length_in_analysis))
specificity <- matrix(nrow = length(required_n_disp_per_drug), ncol = length(rx_length_in_analysis))


# Predict polipharmacy status ----

output_table <- function() {


for (i in seq_along(rx_length_in_analysis)) {
  
  # Define true status (polypharmacy), regardless the number of dispensations ----
  
  # remove overlapping exposure periods within the same dimension because not allowded in the time slicer
  sim_data <- sim_data %>% 
    arrange(id, drug, exp_start) %>% 
    mutate(length = rx_length_in_analysis[i],
           exp_start_v2 = as.Date(exp_start, origin = "2023-01-01"),
           exp_end = exp_start + rx_length_in_analysis[i]) %>% 
    group_by(id, drug) %>% 
    mutate(length = ifelse(exp_start < lag(exp_end), exp_start-(lag(exp_start)), length)) %>% 
    mutate(length = ifelse(is.na(length), rx_length_in_analysis[i], length))
  
  # drugs <- as.character(sim_data %>% ungroup() %>% distinct(drug))
  drugs <- unique(sim_data$drug)
  
  for (k in drugs) {
    
    tmp <- compute.treatment.episodes(sim_data %>% filter(drug==k),
                                      ID.colname = "id",
                                      event.date.colname = "exp_start_v2",
                                      event.duration.colname = "length",
                                      medication.change.means.new.treatment.episode = FALSE,
                                      maximum.permissible.gap = 0,
                                      maximum.permissible.gap.unit = "days",
                                      followup.window.start = as.Date("2023-01-01"),
                                      followup.window.duration = 365,
                                      followup.window.duration.unit = "days")
    
    tmp_v2 <- tmp %>% mutate(episode.start = as.numeric(episode.start - as.Date("2023-01-01")),
                             episode.end = as.numeric(episode.end - as.Date("2023-01-01")),
                             drug = k)
    
    assign(paste("tmp",k, sep = "_"), tmp_v2)
    
  }
  
  sim_data_final <- tmp_A[FALSE,]
  
  for (k in drugs) {
    
    sim_data_final <- rbind(sim_data_final, get(paste("tmp", k, sep = "_")))
    
  }
  
  sim_data_final <- sim_data_final %>% 
    mutate(obs_start = 0,
           obs_end = 365) %>% 
    select(-c(episode.ID, end.episode.gap.days, episode.duration))
  
  # create a fake state variable
  sim_data_final <- sim_data_final %>% 
    mutate(state = 1)
  
  data_sliced <- timeslicer(dt = as.data.table(sim_data_final), 
                            col_id = "id",
                            col_wstart = "obs_start",
                            col_wend = "obs_end",
                            col_epstart = "episode.start",
                            col_epend = "episode.end",
                            col_dim = "drug",
                            col_state = "state")
  
  # count number of drugs per period
  data_sliced <- data_sliced %>% 
    mutate(number_drugs = rowSums(data_sliced %>% select(-c(id, From, To)))) 
  
  def_true <- data_sliced %>% 
    group_by(id) %>% 
    summarise(max_num_drugs = max(number_drugs)) %>% 
    mutate(is_true_polypharmacy = ifelse(max_num_drugs >= 5, 1, 0))
  
  
  for (j in seq_along(required_n_disp_per_drug)) {
    
    
             sim_data_tmp <- sim_data %>% 
               mutate(exp_end = exp_start + rx_length_in_analysis[i])
             
             sim_data_tmp <- sim_data_tmp %>% 
               arrange(id, drug, exp_start) %>% 
               mutate(length = rx_length_in_analysis[i]) %>% 
               mutate(exp_start_v2 = as.Date(exp_start, origin = "2023-01-01")) %>% 
               group_by(id, drug) %>% 
               mutate(length = ifelse(exp_start < lag(exp_end), exp_start-(lag(exp_start)), length)) %>% 
               mutate(length = ifelse(is.na(length), rx_length_in_analysis[i], length))
             
             
             # filter number of dispensations
             count_disp <- sim_data_tmp %>% 
               count(id, drug)
             
             sim_data_tmp <- sim_data_tmp %>% 
               inner_join(count_disp, by = c("id", "drug")) %>% 
               filter(n >= required_n_disp_per_drug[j])
             
             # count number of drugs per subject
             pred_pol <- sim_data_tmp %>% 
                                 group_by(id) %>% 
                                 summarise(n_drugs = n_distinct(drug)) %>% 
                                 mutate(is_pred_polypharmacy = ifelse(n_drugs >= 5, 1, 0))
             
             acc <- pred_pol %>% 
                       right_join(def_true, by = "id") %>% 
                       mutate(is_pred_polypharmacy = ifelse(is.na(is_pred_polypharmacy), 0, is_pred_polypharmacy))
                    
             
             sensitivity[i, j] <- as.numeric(acc %>% 
                                      summarise(sens = round(sum(is_pred_polypharmacy==1 & is_true_polypharmacy==1) / sum(is_true_polypharmacy==1), 2)))
             
             specificity[i, j] <- as.numeric(acc %>% 
                                      summarise(spec = round(sum(is_pred_polypharmacy==0 & is_true_polypharmacy==0) / sum(is_true_polypharmacy==0), 2)))
            
             accuracy <- list(sensitivity, specificity)
             
  }
  
 }
  
  return(accuracy)

}

t <- output_table()
