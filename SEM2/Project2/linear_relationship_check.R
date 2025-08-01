source("SEM2/Project2/LoadData.R")

cols = colnames(data)

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1))
for (i in 1:length(cols)) {
  if (!(cols[i] %in% c("demand_gross", "Date", "year"))) {
    plot(data[[cols[i]]], data$demand_gross, 
         xlab = cols[i], ylab = "demand_gross")
  }
}
