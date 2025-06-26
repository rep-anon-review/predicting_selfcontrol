### Collection of all functions used


# Item selection ----------------------------------------------------------

perform_elastic_net <- function(task) {
  
  el <- lrn("regr.glmnet") 
  imputer <- po("imputemean") # mean imputation for predictor variables 
  scaler <- po("scale")   # standardize the predictor variables
  learner <- as_learner(imputer %>>% scaler %>>% el) 
  
  pipeline_ps <- paradox::ps(
    regr.glmnet.lambda = p_dbl(lower = 0, upper = 1),
    regr.glmnet.alpha  = p_dbl(lower = 0, upper = 1)
  )
  
  res_inner <- rsmp("cv", folds = 5) 
  tuner <- tnr("grid_search", resolution =2000) 
  mes_inner <- msr("regr.mse")
  terminator <- trm("evals")
  pipeline_at <- AutoTuner$new(
    learner = learner, 
    resampling = res_inner, 
    measure = mes_inner, 
    search_space = pipeline_ps, 
    terminator = terminator, 
    tuner = tuner,
    store_models = FALSE
  )
  n_folds <- 10 
  res_outer <- rsmp("cv", folds = n_folds)
  
  invisible({ # supress output 
    capture.output({
      nested_res <- resample(task = task, learner = pipeline_at, resampling = res_outer, store_models = TRUE)
    })
  })
  
  # Performance criteria
  mes <- msr("regr.mse")
  rsq <- msr("regr.rsq")
  
  
  # mes 
  individual_mes <- round(nested_res$score(mes)$regr.mse,3)
  aggregated_mes <- nested_res$aggregate(mes)
  
  # r-square
  individual_rsq <- round(nested_res$score(rsq)$rsq,3)
  aggregated_rsq <- nested_res$aggregate(rsq)
  
  # correlation between observed and predicted values 
  predictions_list <- nested_res$predictions()
  cor_values <- lapply(predictions_list, function(pred) {
    cor(pred$truth, pred$response)
  })
  individual_cor <- round(unlist(cor_values), 3)
  
  # Fisher's z-transformation for average correlation
  z_values <- 0.5 * log((1 + individual_cor) / (1 - individual_cor)) 
  mean_z <- mean(z_values, na.rm = TRUE) # Average of z-values
  # Inverse Fisher's transformation
  aggregated_cor <- (exp(2 * mean_z) - 1) / (exp(2 * mean_z) + 1)
  
  
  #inspecting coefficients
  trained_models <- nested_res$learners
  coef_ls <- list()
  vec_folds <- seq(1, n_folds, 1)
  for(n_f in vec_folds){
    model <- trained_models[[n_f]]$model[[1]] 
    coef_model <- model$model
    coef_output <- coef_model$regr.glmnet$model$beta %>% as.matrix() %>% as.data.frame()  
    names(coef_output) <- paste0("coef")
    coef_output$vars <- row.names(coef_output) 
    coef_ls[[paste0("all_", n_f)]] <- coef_output
    # only consider rows where the coefficient is not zero 
    coef_ls[[paste0("no_zero_", n_f)]] <- coef_output %>% filter(pull(., 1)!=0) 
  }
  
  
  # save hyperparameter 
  all_lambdas <-c()
  for(n_k in vec_folds) {
    all_lambdas[[n_k]] <- nested_res$learners[[1]]$tuning_result$regr.glmnet.lambda
  }
  lambdas <- round(unlist(all_lambdas),4)
  
  all_alphas <-c()
  for(n_k in vec_folds) {
    all_alphas[[n_k]] <- nested_res$learners[[n_k]]$tuning_result$regr.glmnet.alpha 
    
  }
  
  alphas <- round(unlist(all_alphas), 4)
  
  # Return the results
  list(
    performance = list(
      mse = list(individual = individual_mes, aggregated = aggregated_mes),
      rsq = list(individual = individual_rsq, aggregated = aggregated_rsq),
      correlation = list(individual = individual_cor, aggregated =aggregated_cor)
    ),
    coefficients = coef_ls,
    hyperparameters = list(lambda = lambdas, alpha = alphas)
  )
}


# Calculate the average performance of the elastic net output 
calculate_average_perform <- function(results_list) {
  total_mse <- total_rsq <- 0
  total_z_cor <- 0
  num_reps <- 10
  
  for(i in 1:num_reps) {
    rep_name <- paste0("rep_", i)
    
    mse_value <- results_list[[rep_name]]$performance$mse$aggregated
    rsq_value <- results_list[[rep_name]]$performance$rsq$aggregated
    cor_value <- results_list[[rep_name]]$performance$correlation$aggregated
    
    total_mse <- total_mse + mse_value
    total_rsq <- total_rsq + rsq_value
    
    z_cor_value <- 0.5 * log((1 + cor_value) / (1 - cor_value))
    total_z_cor <- total_z_cor + z_cor_value
  }
  
  avg_mse <- round(total_mse / num_reps,3)
  avg_rsq <- round(total_rsq / num_reps,3)
  avg_z_cor <- total_z_cor / num_reps
  avg_cor <- round((exp(2 * avg_z_cor) - 1) / (exp(2 * avg_z_cor) + 1),3)
  
  list(avg_mse = avg_mse, avg_rsq = avg_rsq, avg_cor = avg_cor)
}



unregularized_analysis <- function(data_set, outcome_var) {
  # Create a task
  target <- sym(outcome_var)  
  data_set <- data_set %>% filter(!is.na(!!target))
  task <- as_task_regr(data_set, target = outcome_var)
  
  # Store results for each repetition
  results_list <- list()
  
  for (j in 1:10) {
    # Perform resampling
    imputer <- po("imputemean")
    lm<- lrn("regr.lm")
    scaler <- po("scale")
    learner <- as_learner(imputer %>>% scaler %>>% lm)

    # Fully suppress all console output
    res <- suppressMessages(suppressWarnings(
      resample(learner = learner, task = task, resampling = rsmp("cv", folds = 10))
    ))

    # Correlation with Fisher's z-transformation
    predictions_list <- res$predictions()
    cor_values <- lapply(predictions_list, function(pred) {
      cor(pred$truth, pred$response)
    })
    z_values <- 0.5 * log((1 + unlist(cor_values)) / (1 - unlist(cor_values)))
    mean_z <- mean(z_values, na.rm = TRUE)
    aggregated_cor <- (exp(2 * mean_z) - 1) / (exp(2 * mean_z) + 1)
    
    # R-squared and MSE
    mean_rsq <- res$aggregate(msr("regr.rsq"))
    mean_mse <- res$aggregate(msr("regr.mse"))
    
    # Store results
    results_list[[j]] <- list(cor = aggregated_cor, rsq = mean_rsq, mse = mean_mse)
  }
  
  
  # Return the list of results
  return(results_list)
}

# compute averages of the output of the unregularized analysis 
performance_comparison<- function(result_list) {
  total_z_cor <- total_rsq <- total_mse <- 0
  for (r in 1:10) {
    res <- result_list[[r]]
    z_cor <- 0.5 * log((1 + res$cor) / (1 - res$cor))
    total_z_cor <- total_z_cor + z_cor
    total_rsq <- total_rsq + res$rsq
    total_mse <- total_mse + res$mse
  }
  
  n <- length(result_list)
  avg_z_cor <- total_z_cor / n
  avg_cor <- (exp(2 * avg_z_cor) - 1) / (exp(2 * avg_z_cor) + 1)
  
  list(
    avg_cor = round(avg_cor, 3),
    avg_rsq = round(total_rsq / n, 3),
    avg_mse = round(total_mse / n, 3)
  )
}



# Final analysis ----------------------------------------------------------

prepare_vars <- function(data, single_items, groups) {
  
  # Filter single_items to include only those that are in the data
  existing_single_items <- single_items[names(single_items) %in% colnames(data)]
  
  # Rename the specified single items
  data <- data %>%
    rename_with(~ existing_single_items[.x], .cols = names(existing_single_items))
  
  # Compute means for each group of items and add them as new columns
  for (group_name in names(groups)) {
    group_cols <- groups[[group_name]]
    # Extract the necessary columns first, then compute row means
    mean_data <- rowMeans(data[, all_of(group_cols), drop = FALSE], na.rm = TRUE)
    
    # Now mutate the new column onto the data
    data <- data %>%
      mutate(!!group_name := mean_data)
  }
  
  return(data)
}

final_cors <- function(data, outcome_var) {
  final_dat <- data
  all_vars <- names(final_dat)
  # outcome_variable <- "m_success_pers_ESM"
  predictor_vars <- all_vars[!all_vars %in% outcome_var]
  cor_output <- cor(final_dat[outcome_var], final_dat[predictor_vars], use = "complete.obs") 
  result <- data.frame(
    var = colnames(cor_output),
    coefficient = as.numeric(cor_output)
  ) %>% mutate(abs_coefficient = abs(coefficient),
               direction = ifelse(coefficient > 0, "positive", "negative"))
  return(result)
}

final_betas <- function(data, outcome_var) {
  all_vars <- names(data)
  predictor_vars <- setdiff(all_vars, outcome_var)
  
  # Keep only rows where outcome is not missing
  data_filtered <- data[!is.na(data[[outcome_var]]), ]
  
  # Mean imputation for missing predictor values
  for (var in predictor_vars) {
    missing <- is.na(data_filtered[[var]])
    if (any(missing)) {
      data_filtered[[var]][missing] <- mean(data_filtered[[var]], na.rm = TRUE)
    }
  }
  
  # Standardize all variables
  data_scaled <- data_filtered %>%
    mutate(across(all_of(c(outcome_var, predictor_vars)), scale))
  
  # Run regression
  formula <- as.formula(paste(outcome_var, "~", paste(predictor_vars, collapse = " + ")))
  model <- lm(formula, data = data_scaled)
  
  # Extract coefficients (excluding intercept)
  betas <- coef(model)[-1]
  
  result <- data.frame(
    var = names(betas),
    coefficient = as.numeric(betas)
  ) %>% mutate(
    abs_coefficient = abs(coefficient),
    direction = ifelse(coefficient > 0, "positive", "negative")
  )
  
  return(result)
}


coefficient_plot<- function(stats_df, title = "", left_margin = 9,  x_limits = c(0, .45)) {
  max_bars <- 10  # The maximum number of predictors used across all plots
  
  if (nrow(stats_df) == 0) {
    par(mar = c(3, left_margin, 3, 2))  # Ensure margins match bar plots
    plot(0, type = "n", xlab = "", ylab = "", 
         xlim = x_limits, ylim = c(0.5, max_bars+ 0.5),  
         axes = FALSE, main = title, cex.main = 1.5)
    
    # Move the text slightly up & keep consistent scaling
    text(mean(x_limits) * 1, max_bars / 2,  
         "Selection of predictors was not \npossible as the predictive \n performance of the model \n was too poor",
         cex = 1.2, font = 1.5, adj = 0.5)
    
    #  Ensure y-axis structure is identical to real plots
    axis(2, at = seq(1, max_bars), labels = rep("", max_bars), tck = 0, col = "black", lwd = 2)  
    axis(1, at = pretty(x_limits), cex.axis = 1.3, col = "black", lwd = 2)
    
    return()  # Ensures nothing else is drawn after this
  }
  
  
  #order predictors by absolute coefficient size
  stats_df <- stats_df[order(-stats_df$abs_coefficient), ]  
  
  # Compute bar positions, keeping the same spacing regardless of the number of predictors
  all_positions <- seq(from = max_bars, to = 1, by = -1)  # Always use all positions
  num_predictors <- nrow(stats_df)  # Actual number of predictors
  empty_slots <- max_bars - num_predictors  # Empty space slots at the bottom
  
  # Assign bars to fixed positions, leaving blank space at the bottom for consistency
  bar_positions <- all_positions[1:num_predictors]  
  
  # Set up the plot with fixed y-axis and equal spacing for all plots
  par(mar = c(3, left_margin, 3, 2))  
  plot(0, type = "n", xlim = x_limits, ylim = c(0.5, max_bars + 0.5), 
       xlab = "", ylab = "", axes = FALSE, main = title, cex.main = 1.5)
  
  # Add bars with fixed thickness
  bar_colors_used <- ifelse(stats_df$direction == "positive", bar_colors[2], bar_colors[1])
  rect(0, bar_positions - 0.4, stats_df$abs_coefficient, bar_positions + 0.4, col = bar_colors_used, border = "black")
  
  # Add y-axis labels for all 10 slots (even if some are empty)
  axis_labels <- rep("", max_bars)  # Create an empty label list
  axis_labels[1:num_predictors] <- stats_df$var  # Fill in present predictors
  axis(2, at = seq(1, max_bars), labels = NA, tck = 0, col = "black", lwd = 2)  # Draws full y-axis without extra ticks
  axis(2, at = bar_positions, labels = stats_df$var, las = 1, cex.axis = 1.2, tck = -0.02)  # Adds ticks & labels only for present predictors
  
  # Add x-axis with consistent limits
  axis(1, at = pretty(x_limits), cex.axis = 1.2, col ="black", lwd= 2)
  
  
}


# ABCD coding -------------------------------------------------------------

ABCD_prep <- function(data, groups, single_items) {
  # Ensure that single_items is not NULL, and is a named list, if not skip renaming
  if (!is.null(single_items) && length(single_items) > 0 && !all(is.na(names(single_items)))) {
    # Filter and rename only if single_items is correctly specified
    singles <- data %>%
      filter(!(item_name %in% unlist(groups))) %>%
      mutate(item_name = ifelse(item_name %in% names(single_items), single_items[item_name], item_name))
  } else {
    # Proceed without renaming if single_items is empty or not correctly specified
    singles <- data %>%
      filter(!(item_name %in% unlist(groups)))
  }
  
  # Initialize an empty list to store results of group computations
  composites <- list()
  
  # Calculate means for each group and store results
  for (group_name in names(groups)) {
    # Filter data for the current group and summarize
    group_data <- data %>%
      filter(item_name %in% groups[[group_name]]) %>%
      summarise(across(c(affect, behavior, cognition, desire), mean, na.rm = TRUE)) %>%
      mutate(item_name = group_name)  # Assign the group name for identification
    
    # Append the computed data to the results list
    composites[[group_name]] <- group_data
  }
  
  # Bind all composite data into one dataframe
  composites <- dplyr::bind_rows(composites)
  
  # Combine single items and group results into one dataset
  combined <- dplyr::bind_rows(singles, composites)
  
  return(combined)
}

