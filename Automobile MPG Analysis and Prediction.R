library(tidyverse)
library(ggplot2)
data<- read.csv("C:/Users/Aishu/Downloads/auto-mpg.csv")

# Remove symbols or non-numeric characters from relevant columns
data$horsepower <- as.numeric(gsub("[^0-9.]", "", data$horsepower))
data$weight <- as.numeric(gsub("[^0-9.]", "", data$weight))
# Repeat similar cleaning for other columns as needed

# Remove missing values
auto_data <- na.omit(data)

# Visualizations
# Scatter plot - Horsepower vs. MPG
ggplot(auto_data, aes(x = horsepower, y = mpg)) +
  geom_point() +
  labs(title = "Scatter Plot - Horsepower vs. MPG",
       x = "Horsepower",
       y = "MPG")

# Boxplot - Cylinders vs. MPG
ggplot(auto_data, aes(x = as.factor(cylinder), y = mpg)) +
  geom_boxplot() +
  labs(title = "Boxplot - Cylinder vs. MPG",
       x = "Number of Cylinders",
       y = "MPG")

# Histogram - MPG distribution
ggplot(auto_data, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "darkblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram - MPG Distribution",
       x = "MPG",
       y = "Frequency")



# Take the first 300 samples
auto_data_new <- auto_data[1:300, ]

# Simple Linear Regression
simp_lm <- lm(mpg ~ horsepower, data = auto_data_new)
summary(simp_lm)

# Multiple Linear Regression
multi_lm <- lm(mpg ~ cylinder + displacement + horsepower + weight + acceleration + origin, data = auto_data_new)
# for summary
summary(multi_lm)

log <- data.frame(
  Model = c("Simple Linear Regression", "Multiple Linear Regression"),
  Multi_R_sq = c(summary(simp_lm)$r.squared, summary(multi_lm)$r.squared),
  Adj_R_sq = c(summary(simp_lm)$adj.r.squared, summary(multi_lm)$adj.r.squared),
  Comp_Lm_Equation = c(paste("mpg =", round(simp_lm$coefficients[1], 4), "+", round(simp_lm$coefficients[2], 4), "* horsepower"),
                                          paste("mpg =", round(multi_lm$coefficients[1], 4), "+", 
                                                paste( round(multi_lm$coefficients[-1], 4), names(multi_lm$coefficients[-1]), collapse = " + ")))
)

# Print the log
print(log)



# Select the remaining 98 samples
remain_data <- tail(auto_data, 98)

# Predict using the multiple linear regression model
predict_mpg <- predict(multi_lm, newdata = remain_data)

# Calculate residuals
residuals <- remain_data$mpg - predict_mpg

# Residual Plot
plot(predict_mpg, residuals, main = "Residual Plot", xlab = "Predicted MPG", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# Histogram of Residuals
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "orange", border = "black")





