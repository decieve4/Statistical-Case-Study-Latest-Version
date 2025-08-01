source("SEM2/Project2/data_preprocessing.R")

point_size <- 0.5
point_style <- 19
scatter_col <- rgb(0.2, 0.4, 0.6, 0.5)
box_col_fill <- rgb(0.2, 0.4, 0.6, 0.2)
box_col_border <- rgb(0.2, 0.4, 0.6, 0.6)
axis_col <- "gray90"



# 1. solar_S vs demand_gross
plot(data$solar_S, data$demand_gross,
     xlab = "solar_S (Solar Capacity Factor)",
     ylab = "Demand (MW)",
     xlim = c(0, 0.5),
     pch = 16,
     cex = 0.5,
     col = rgb(31/255, 119/255, 180/255, 0.75),
     main = "")
grid(col = "gray85", lty = "dotted")
box(lwd = 0.6)

# 2. wdayindex vs demand_gross (boxplot)
boxplot(demand_gross ~ wdayindex, data = data,
        xlab = "Weekday Index (0 = Sun, ..., 6 = Sat)",
        ylab = "Demand (MW)",
        col = rgb(44/255, 160/255, 44/255, 0.6),
        border = rgb(44/255, 160/255, 44/255, 0.9),
        frame = FALSE,
        main = "")
grid(col = "gray85", lty = "dotted")
box(lwd = 0.6)


# start_year vs demand
boxplot(demand_gross ~ start_year, data = data,
        col = rgb(148/255, 103/255, 189/255, 0.6),
        border = rgb(148/255, 103/255, 189/255, 0.9),
        xlab = "Winter Start Year",
        ylab = "Demand (MW)",
        cex.axis = 0.8,
        main = "")
grid(col = "gray85", lty = "dotted")
box(lwd = 0.6)



# DSN vs demand
boxplot(demand_gross ~ DSN, data = data,
        col = rgb(255/255, 127/255, 14/255, 0.6),
        border = rgb(255/255, 127/255, 14/255, 0.9),
        xlab = "DSN (Day Index Since Nov 1)",
        ylab = "Demand (MW)",
        outline = FALSE,
        las = 2,
        cex.axis = 0.6,
        main = "")
grid(col = "gray85", lty = "dotted")
box(lwd = 0.6)
