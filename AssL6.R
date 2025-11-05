# --- Load required library ---
library(caTools)

# --- Load dataset ---
data <- read.csv("C:/Users/Kasturi/Downloads/crime_safety_dataset.csv")

# --- Clean data ---
data <- na.omit(data)

# --- Encode 'crime_type' as numeric for correlation/regression ---
# (Convert factor/categorical variable to numeric representation)
data$crime_type_code <- as.numeric(as.factor(data$crime_type))

# --- Check numeric relationship between victim_age and crime_type_code ---
cov_val <- cov(data$victim_age, data$crime_type_code)
cor_val <- cor(data$victim_age, data$crime_type_code)
cat("Covariance:", cov_val, "\n")
cat("Correlation:", cor_val, "\n")

# --- Split into training and testing sets ---
set.seed(123)
split <- sample.split(data$victim_age, SplitRatio = 0.7)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# --- Compute mean deviation for train/test ---
dev_train <- mean(train$victim_age - mean(train$victim_age))
dev_test <- mean(test$victim_age - mean(test$victim_age))
cat("Deviation Train:", dev_train, "\n")
cat("Deviation Test:", dev_test, "\n")

# --- Linear regression model ---
# Here we model: victim_age ~ crime_type_code
model <- lm(victim_age ~ crime_type_code, data=train)
summary(model)

# --- Predict on test set ---
pred <- predict(model, newdata=test)

# --- Plot results ---
plot(test$crime_type_code, test$victim_age,
     col="blue", pch=16,
     main="Linear Regression: Crime Type vs Victim Age",
     xlab="Crime Type (encoded)", ylab="Victim Age")
abline(model, col="red", lwd=2)
