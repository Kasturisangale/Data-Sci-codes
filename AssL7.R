if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("Metrics", quietly = TRUE)) install.packages("Metrics")

library(readr)
library(caret)
library(Metrics)

data <- read.csv("C:/Users/Kasturi/Downloads/crime_safety_dataset.csv")
data <- na.omit(data)

data$crime_type <- as.numeric(as.factor(data$crime_type))
data$city <- as.numeric(as.factor(data$city))
data$state <- as.numeric(as.factor(data$state))

features <- c("crime_type", "city", "state")
target <- "victim_age"
formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))

set.seed(42)
trainIndex <- createDataPartition(data[[target]], p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

model <- lm(formula, data = trainData)
summary(model)

predictions <- predict(model, newdata = testData)

r2 <- R2(predictions, testData[[target]])
rmse_val <- rmse(testData[[target]], predictions)
mse_val <- mse(testData[[target]], predictions)

cat("R² Score:", r2, "\n")
cat("RMSE:", rmse_val, "\n")
cat("MSE:", mse_val, "\n")

plot(testData[[target]], predictions,
     col = "blue", pch = 16,
     main = "Predicted vs Actual Victim Age",
     xlab = "Actual Victim Age", ylab = "Predicted Victim Age")
abline(0, 1, col = "red", lwd = 2)
