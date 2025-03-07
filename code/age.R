# Histogram for age distribution
data <- read.csv("data/cat-fetching.csv")
hist(cat_data$age, main = "Sample Age Distribution", xlab = "Age", col = "lightblue", border = "gray")
