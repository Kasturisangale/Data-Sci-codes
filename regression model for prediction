required_pkgs <- c("caret","dplyr","e1071","infotheo","corrplot")
to_install <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install, repos='https://cloud.r-project.org')

library(caret)
library(dplyr)
library(e1071)
library(infotheo)
library(corrplot)

data_path <- "C:/Users/Kasturi/Downloads/crime_safety_dataset.csv"
df <- read.csv(data_path, stringsAsFactors = FALSE)
cat("Dataset loaded successfully.\n\n")

set.seed(123)
df$crime_severity <- sample(c("High", "Low"), nrow(df), replace = TRUE, prob = c(0.6, 0.4))
df$crime_severity <- as.factor(df$crime_severity)
cat("Dummy target variable 'crime_severity' added.\n\n")

df[df == ""] <- NA
df <- na.omit(df)
cat("After handling missing values, dimensions:", dim(df)[1], "x", dim(df)[2], "\n\n")

cat("===== Descriptive Statistics for Numeric Columns =====\n")
num_cols <- sapply(df, is.numeric)
summary_stats <- data.frame(
  Mean = sapply(df[, num_cols, drop=FALSE], mean, na.rm = TRUE),
  Median = sapply(df[, num_cols, drop=FALSE], median, na.rm = TRUE),
  Mode = sapply(df[, num_cols, drop=FALSE], function(x){
    ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
  }),
  IQR = sapply(df[, num_cols, drop=FALSE], IQR, na.rm = TRUE)
)
print(round(summary_stats, 4))
cat("\n")

cat("===== Correlation Matrix =====\n")
num_names <- names(df)[sapply(df, is.numeric)]
if(length(num_names) >= 2){
  cor_matrix <- cor(df[, num_names], use = "complete.obs")
  print(round(cor_matrix, 3))
  corrplot(cor_matrix, method="color", type="upper", tl.col="black", tl.srt=45)
} else {
  cat("Not enough numeric columns for correlation matrix.\n")
}
cat("\n")

set.seed(123)
trainIndex <- createDataPartition(df$crime_severity, p = 0.7, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]
cat("Train rows:", nrow(train), " Test rows:", nrow(test), "\n\n")

cat("===== Logistic Regression Model =====\n")
df$crime_type <- as.numeric(as.factor(df$crime_type))
df$city <- as.numeric(as.factor(df$city))
df$state <- as.numeric(as.factor(df$state))
df$victim_gender <- as.numeric(as.factor(df$victim_gender))
df$victim_race <- as.numeric(as.factor(df$victim_race))

predictors <- c("crime_type", "city", "state", "victim_age", "victim_gender", "victim_race")
formula_str <- paste("crime_severity ~", paste(predictors, collapse = " + "))
cat("Model formula:", formula_str, "\n\n")

model <- glm(as.formula(formula_str), data = train, family = binomial)

pred_prob <- predict(model, test, type = "response")
pred <- as.factor(ifelse(pred_prob > 0.5, "High", "Low"))
confusion <- confusionMatrix(pred, test$crime_severity, positive="High")

cat("===== Confusion Matrix & Metrics =====\n")
print(confusion)

train_pred_prob <- predict(model, train, type = "response")
train_pred <- as.factor(ifelse(train_pred_prob > 0.5, "High", "Low"))
train_acc <- mean(train_pred == train$crime_severity)
test_acc  <- mean(pred == test$crime_severity)
cat("\nTrain Accuracy:", train_acc, "\n")
cat("Test Accuracy:", test_acc, "\n")
cat("Model Fit Status:",
    if (train_acc - test_acc > 0.05) "Possibly overfitted"
    else if (train_acc - test_acc < -0.05) "Possibly underfitted"
    else "Well-fitted", "\n\n")

cat("===== Entropy & Information Gain =====\n")
entropy_target <- entropy(table(df$crime_severity))
info_gain <- mutinformation(df$crime_severity, df$victim_gender)
cat("Entropy of Target:", entropy_target, "\n")
cat("Information Gain (crime_severity ~ victim_gender):", info_gain, "\n\n")

cat("===== Model Summary =====\n")
print(summary(model))

cat("\nConclusion:\n")
cat("Thus we learnt Crime Severity Prediction using Logistic Regression and Statistical Analysis.\n")
