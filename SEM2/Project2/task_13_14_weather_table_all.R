year_seq = unique(data$start_year)

table_df <- data.frame(
  Year = numeric(),
  Annual_Demand = numeric(),
  Lower_Bound = numeric(),
  Upper_Bound = numeric(),
  stringsAsFactors = FALSE
)

get_mode_continuous <- function(x) {
  kde <- density(x)
  mode_value <- kde$x[which.max(kde$y)]
  return(mode_value)
}

for (year in year_seq) {
  this_year_demand_prediction <- results_list[[year]]
  
  scalar_demand <- as.integer(get_mode_continuous(this_year_demand_prediction$annual_demand))
  scalar_lb <- as.integer(get_mode_continuous(this_year_demand_prediction$lower_bound))
  scalar_ub <- as.integer(get_mode_continuous(this_year_demand_prediction$upper_bound))
  
  year_row <- data.frame(
    Year = year,
    Annual_Demand = scalar_demand,
    Lower_Bound = scalar_lb,
    Upper_Bound = scalar_ub
  )
  
  table_df <- rbind(table_df, year_row)
}

print(xtable(table_df), include.rownames = FALSE)

threshold = 10000

table_df$label <- ifelse(table_df$Annual_Demand > ground_truth_annual_demand + threshold, 
                         "bigger", 
                         ifelse(table_df$Annual_Demand < ground_truth_annual_demand - threshold, 
                                "smaller", 
                                "similar"))

