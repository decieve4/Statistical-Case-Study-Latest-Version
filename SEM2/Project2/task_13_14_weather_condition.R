source("SEM2/Project2/task_temperature.R")
library(dplyr)

block_bootstrap_sample <- function(blocks, num_blocks) {
  indices <- sample(seq_len(num_blocks), size = num_blocks, replace = TRUE)
  boot_data <- do.call(rbind, blocks[indices])
  return(boot_data)
}

calc_model_metrics <- function(model, data) {
  pred_results <- predict(model, newdata = data, interval = "prediction", level = 0.95) # we want the predictive distribution result
  preds <- pred_results[, 1]
  lower_bounds <- pred_results[, 2]
  upper_bounds <- pred_results[, 3]
  
  return(
    c(
      annual_demand = sum(preds), 
      lower_bound = sum(lower_bounds), 
      upper_bound = sum(upper_bounds)
      )
    )
}

block_size <- 7
B <- 100
n <- nrow(data) 
n_full_block <- floor(n / block_size) * block_size
df_sub <- data[1:n_full_block, ]
block_list <- split(df_sub, rep(1:(n_full_block / block_size), each = block_size))
num_blocks <- length(block_list)

length_of_all_years <- length(unique(data$start_year))
results_list <- list()

# Loop through each unique year in 'data$start_year'
for (year in unique(data$start_year)) {
  metric_df <- data.frame(annual_demand = numeric(0), lower_bound = numeric(0), upper_bound = numeric(0))
  
  # Filter weather conditions for the year in question
  weather_condition <- data[data$start_year == year, c("wind", "TE_AVG_10_15", "DSN")]
  
  # Prepare the dataset for 2013, including weather conditions from the current year
  dataset <- data %>%
    filter(start_year == 2013) %>%
    select(wdayindex, start_year, DSN) %>%
    inner_join(weather_condition, by = "DSN")
  
  for (b in seq_len(B)) {
    boot_data <- block_bootstrap_sample(block_list, num_blocks)
    fit <- lm(demand_gross ~ start_year + wdayindex + wind + TE_AVG_10_15 + DSN, data = boot_data)
    metrics <- calc_model_metrics(fit, dataset)
    
    metric_df <- rbind(metric_df, data.frame(annual_demand = metrics[1], lower_bound = metrics[2], upper_bound = metrics[3]))
  }
  
  results_list[[as.character(year)]] <- metric_df
}

ground_truth_annual_demand <- data %>%
  filter(start_year == 2013) %>%
  pull(demand_gross) %>%
  sum(na.rm = TRUE)
