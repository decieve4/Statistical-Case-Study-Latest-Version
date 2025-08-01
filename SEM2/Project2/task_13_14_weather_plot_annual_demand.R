library(ggplot2)

year = "2011"

data_some_year <- data.frame(
  lower_bound = results_list[[year]]$lower_bound,
  upper_bound = results_list[[year]]$upper_bound,
  annual_demand = results_list[[year]]$annual_demand
)

ggplot() +
  # lb
  geom_histogram(data = data_some_year, aes(x = lower_bound, fill = "Lower Bound", y = ..density..), 
                 alpha = 0.5, bins = 50, color = "black") +
  # ub
  geom_histogram(data = data_some_year, aes(x = upper_bound, fill = "Upper Bound", y = ..density..), 
                 alpha = 0.5, bins = 50, color = "black") +
  # y
  geom_histogram(data = data_some_year, aes(x = annual_demand, fill = "Annual Demand", y = ..density..), 
                 alpha = 0.5, bins = 50, color = "black") +
  # ground truth demand in 2013
  geom_vline(aes(xintercept = ground_truth_annual_demand, color = "Annual Demand in 2013-2014"), 
             linetype = "dashed", size = 1.5) +
  labs(x = "Demand", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("Lower Bound" = "blue", "Upper Bound" = "red", "Annual Demand" = "orange")) +
  scale_color_manual(values = c("Annual Demand in 2013-2014" = "red")) +
  theme(legend.position = "top") + 
  guides(fill = guide_legend(title = "Predictive Distribution"), color = guide_legend(title = "Baseline"))
