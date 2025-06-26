
# Load packages and data -----------------------------------------------------------
library(mlr3verse)
library(paradox)
library(mlr3tuning)
library(glmnet)
library(DALEX)
library(tidyverse)
library(here)
library(purrr)
library(checkmate)
library(readr)
library(rpart.plot) 
library(future)

source(here("Scripts", "01_functions.R"))

data <- read_csv(here("Data", "prep_data.csv"))


# Specify the tasks -------------------------------------------------------

# List of outcome variable names 
ESM_vars <- data %>% dplyr::select(ends_with("ESM"))  %>% names()
dem_vars <- data %>% dplyr::select(Participant, starts_with("Dem")) %>% names()


outcome_vars <- data %>% dplyr::select(ends_with("_ESM")) %>%
  dplyr::select(-starts_with("n_con")) %>%
  dplyr::select(-n_per_person_ESM, -n_any_con_ESM) %>% names()
vars_to_exclude <- c(ESM_vars, dem_vars, "Participant")

# Prepare tasks for each outcome
all_tasks <- lapply(outcome_vars, function(outcome_var) { # change here 
  outcome_var_sym <- sym(outcome_var)
  data_set <- data %>% filter(!is.na(!!outcome_var_sym))
  task <- as_task_regr(data_set, id = paste0("EL_", outcome_var), target = outcome_var)
  task$set_col_roles(vars_to_exclude, remove_from = "feature")
  task
})



# Running elastic net models (takes > 25 hours) ---------------------------
run_elastic_net <- FALSE  # change to TRUE to run the model
# Currently set to FALSE due to the long run time of the model 

if (run_elastic_net) {
  #save all results in one masterlist
  outcome_list <- list()
  set.seed(142837)
  
  plan("multisession", workers = 4)
  options(future.seed = TRUE)
  
  start_time <- Sys.time()
  
  # Loop through each task (outcome)
  for (i in 1:length(all_tasks)) {
    task <- all_tasks[[i]]
    outcome_var <- outcome_vars[i] # change here
    
    # Initialize a list to store the results of the 10 repetitions for this task
    task_results <- list()
    
    # Repeat the modeling process 10 times
    for (j in 1:10) {
      rep_results <- perform_elastic_net(task)
      task_results[[paste0("rep_", j)]] <- rep_results
    }
    
    # Store the results for this outcome in the master list
    outcome_list[[outcome_var]] <- task_results
  }
  
  end_time <- Sys.time()
  total_duration <- end_time - start_time # 25.55 hours
  future::plan("sequential")
  
  
  # save outcome list
  if (!dir.exists("ML results")) dir.create("ML results", recursive = TRUE)
  saveRDS(outcome_list, here("ML results", "elastic_net_results_2304.rds"))
  
}


# Performance check elastic net models ------------------------------------
# load the outcome list if the models were not computed again
outcome_list <- readRDS(here("ML results", "elastic_net_results_2304.rds"))

# components across conflict types
calculate_average_perform(outcome_list$freq_con_all_ESM)
calculate_average_perform(outcome_list$m_intensity_all_ESM)
calculate_average_perform(outcome_list$m_success_all_ESM)

# frequency separately for each conflict type
calculate_average_perform(outcome_list$freq_con_init_ESM)
calculate_average_perform(outcome_list$freq_con_pers_ESM)
calculate_average_perform(outcome_list$freq_con_inhi_ESM)

#intensity separately for each conflict type
calculate_average_perform(outcome_list$m_intensity_init_ESM)
calculate_average_perform(outcome_list$m_intensity_pers_ESM)
calculate_average_perform(outcome_list$m_intensity_inhi_ESM)

# success separately for each conflict type
calculate_average_perform(outcome_list$m_success_init_ESM)
calculate_average_perform(outcome_list$m_success_pers_ESM)
calculate_average_perform(outcome_list$m_success_inhi_ESM)



# Use different predictor sets in unregularized models --------------------

final_var_selection <- list()

# set seed once more in case the analysis are not done in one run
set.seed(1862964130)

# Loop through each outcome
lgr::get_logger("mlr3")$set_threshold("error")  # only show actual errors
for (i in seq_along(outcome_list)) {
  result_list <- outcome_list[[i]]
  outcome_var <- outcome_vars[i]
  
  all_selected_vars <- c()
  
  for (rep in names(result_list)) {
    for (fold in names(result_list[[rep]]$coefficients)) {
      if (grepl("no_zero", fold)) {
        new_vars <- result_list[[rep]]$coefficients[[fold]]$vars
        all_selected_vars <- c(all_selected_vars, new_vars)
      }
    }
  }
  
  #create three predictor sets with items that were selected in 70, 80, or 90 folds 
  var_counts <- table(all_selected_vars)
  vars_70 <- names(var_counts[var_counts >= 70])
  vars_80 <- names(var_counts[var_counts >= 80])
  vars_90 <- names(var_counts[var_counts >= 90])
  
  analyze_subset <- function(vars_subset) {
    if (length(vars_subset) > 0) {
      data_subset <- data %>% 
        dplyr::select(all_of(vars_subset), all_of(outcome_var)) %>% 
        filter(!is.na(outcome_var)) 
      results <- unregularized_analysis(data_subset, outcome_var)
      
    } else {
      list(avg_cor = NA, avg_rsq = NA, avg_mse = NA)
    }
  }
  
  final_var_selection[[outcome_var]] <- list(
    item_sets = list("70%_vars" = vars_70, "80%_vars" = vars_80, "90%_vars" = vars_90),
    performance = list(
      "70%_performance" = analyze_subset(vars_70),
      "80%_performance" = analyze_subset(vars_80),
      "90%_performance" = analyze_subset(vars_90)
    )
  )
}

saveRDS(final_var_selection, here("ML results", "results_item_sets_2304.rds"))


# Compare the performance to get the final item sets ----------------------
# frequency across conflicts 
final_var_selection$freq_con_all_ESM$item_sets 
performance_comparison(final_var_selection$freq_con_all_ESM$performance$`70%_performance`)
performance_comparison(final_var_selection$freq_con_all_ESM$performance$`80%_performance`) 
performance_comparison(final_var_selection$freq_con_all_ESM$performance$`90%_performance`) # performs best
final_vars_frequency_all <- data.frame(final_var_selection$freq_con_all_ESM$item_sets$`90%_vars`)
write_csv(final_vars_frequency_all, here("ML results", "final_var_frequency_all.csv"))

# intensity across conflicts 
final_var_selection$m_intensity_all_ESM$item_sets   
performance_comparison(final_var_selection$m_intensity_all_ESM$performance$`70%_performance`) 
performance_comparison(final_var_selection$m_intensity_all_ESM$performance$`80%_performance`)  
performance_comparison(final_var_selection$m_intensity_all_ESM$performance$`90%_performance`) # performs best
final_vars_intensity_all <- data.frame(final_var_selection$m_intensity_all_ESM$item_sets$`90%_vars`)
write_csv(final_vars_intensity_all, here("ML results", "final_var_intensity_all.csv"))

# success across conflicts 
final_var_selection$m_success_all_ESM$item_sets  
performance_comparison(final_var_selection$m_success_all_ESM$performance$`70%_performance`)
performance_comparison(final_var_selection$m_success_all_ESM$performance$`80%_performance`)
performance_comparison(final_var_selection$m_success_all_ESM$performance$`90%_performance`) # performs best 
final_vars_success_all <- data.frame(final_var_selection$m_success_all_ESM$item_sets$`90%_vars`)
write_csv(final_vars_success_all, here("ML results", "final_var_success_all.csv"))


# frequency initiation
final_var_selection$freq_con_init_ESM$item_sets
performance_comparison(final_var_selection$freq_con_init_ESM$performance$`70%_performance`) # performs best
performance_comparison(final_var_selection$freq_con_init_ESM$performance$`80%_performance`) 
performance_comparison(final_var_selection$freq_con_init_ESM$performance$`90%_performance`) 
final_vars_frequency_init <- data.frame(final_var_selection$freq_con_init_ESM$item_sets$`70%_vars`)
write_csv(final_vars_frequency_init, here("ML results", "final_var_frequency_init.csv"))


# frequency persistence
final_var_selection$freq_con_pers_ESM$item_sets 
performance_comparison(final_var_selection$freq_con_pers_ESM$performance$`70%_performance`) # performs best
performance_comparison(final_var_selection$freq_con_pers_ESM$performance$`80%_performance`)
performance_comparison(final_var_selection$freq_con_pers_ESM$performance$`90%_performance`)
final_vars_frequency_pers <- data.frame(final_var_selection$freq_con_pers_ESM$item_sets$`70%_vars`)
write_csv(final_vars_frequency_pers, here("ML results", "final_var_frequency_pers.csv"))

# frequency inhibition
final_var_selection$freq_con_inhi_ESM$item_sets # all predictor sets are empty  

# intensity initiation
final_var_selection$m_intensity_init_ESM$item_sets # same items were selected in 80% and 90% of the folds 
performance_comparison(final_var_selection$m_intensity_init_ESM$performance$`70%_performance`) 
performance_comparison(final_var_selection$m_intensity_init_ESM$performance$`90%_performance`) # performs best
final_vars_intensity_init <- data.frame(final_var_selection$m_intensity_init_ESM$item_sets$`90%_vars`)
write_csv(final_vars_intensity_init, here("ML results", "final_var_intensity_init.csv"))

# intensity persistence
final_var_selection$m_intensity_pers_ESM$item_sets 
performance_comparison(final_var_selection$m_intensity_pers_ESM$performance$`70%_performance`) 
performance_comparison(final_var_selection$m_intensity_pers_ESM$performance$`80%_performance`) # performs best 
performance_comparison(final_var_selection$m_intensity_pers_ESM$performance$`90%_performance`) 
final_vars_intensity_pers <- data.frame(final_var_selection$m_intensity_pers_ESM$item_sets$`80%_vars`)
write_csv(final_vars_intensity_pers, here("ML results", "final_var_intensity_pers.csv"))

# intensity inhibition
final_var_selection$m_intensity_inhi_ESM$item_sets # all predictor sets are empty


# success initiation
final_var_selection$m_success_init_ESM$item_sets                  
performance_comparison(final_var_selection$m_success_init_ESM$performance$`70%_performance`)
performance_comparison(final_var_selection$m_success_init_ESM$performance$`80%_performance`)
performance_comparison(final_var_selection$m_success_init_ESM$performance$`90%_performance`) # performs best         
final_vars_success_init <- data.frame(final_var_selection$m_success_init_ESM$item_sets$`90%_vars`)       
write_csv(final_vars_success_init, here("ML results", "final_var_success_init.csv"))


# success persistence 
final_var_selection$m_success_pers_ESM$item_sets 
performance_comparison(final_var_selection$m_success_pers_ESM$performance$`70%_performance`)
performance_comparison(final_var_selection$m_success_pers_ESM$performance$`80%_performance`) # performs best 
performance_comparison(final_var_selection$m_success_pers_ESM$performance$`90%_performance`) 
final_vars_success_pers <- data.frame(final_var_selection$m_success_pers_ESM$item_sets$`80%_vars`) 
write_csv(final_vars_success_pers, here("ML results", "final_var_success_pers.csv"))


# success inhibition
final_var_selection$m_success_inhi_ESM$item_sets # all predictor sets are empty







