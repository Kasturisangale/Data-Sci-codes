# ------------------------------
# 1. Import dataset
# ------------------------------
crime_data <- read.csv("C:/Users/Kasturi/Downloads/crime_safety_dataset.csv")
# ------------------------------
# 2. Select only numeric columns
# ------------------------------
crime_numeric <- crime_data[sapply(crime_data, is.numeric)]

# Check numeric columns
print(colnames(crime_numeric))

# Ensure we have at least 2 numeric columns
if (ncol(crime_numeric) < 2) stop("Not enough numeric columns for analysis.")

# ------------------------------
# 3. Correlation between first half vs second half of numeric columns
# ------------------------------
n_num <- ncol(crime_numeric)
split_index <- ceiling(n_num / 2)
x <- as.matrix(crime_numeric[, 1:split_index])
y <- as.matrix(crime_numeric[, (split_index + 1):n_num])

# Correlation matrix between x and y
COR <- cor(x, y, use = "complete.obs")

# Image heatmap
image(x = seq(ncol(x)), y = seq(ncol(y)), z = t(COR),
      xlab = "X columns", ylab = "Y columns",
      col = heat.colors(10))

# ------------------------------
# 4. Correlation plot using corrplot
# ------------------------------
# Install if not already
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# Full correlation matrix for all numeric variables
crime_cor <- cor(crime_numeric, use = "complete.obs")

# Reordered correlation plot
corrplot(crime_cor, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# ------------------------------
# 5. Scatterplot with regression line using ggplot2
# ------------------------------
# Install ggplot2 if needed
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Automatically pick first two numeric columns for scatterplot
x_col <- colnames(crime_numeric)[1]
y_col <- colnames(crime_numeric)[2]

ggplot(data = crime_numeric, aes_string(x = x_col, y = y_col)) +
  geom_point(size = 2, colour = "black") +
  geom_point(size = 1, colour = "white") +
  geom_smooth(aes(colour = "black"), method = "lm") +
  ggtitle(paste(y_col, "vs", x_col)) +
  xlab(x_col) + ylab(y_col) +
  theme(legend.position = "none")
    





# ------------------------------
# 6. Linear Regression Model
# ------------------------------
lm_model <- lm(as.formula(paste(y_col, "~", x_col)), data = crime_numeric)
summary(lm_model)

# Predict values for first 5 rows
predicted_lm <- predict(lm_model, newdata = crime_numeric[1:5, ])
print("Predicted values (first 5 rows) from Linear Regression:")
print(predicted_lm)

# ------------------------------
# 7. Logistic Regression
# ------------------------------
# Create a binary target variable based on median of dependent column
median_val <- median(crime_numeric[[y_col]], na.rm = TRUE)
crime_numeric$target_binary <- ifelse(crime_numeric[[y_col]] > median_val, 1, 0)

# Fit logistic regression using the first numeric column as independent variable
log_model <- glm(target_binary ~ get(x_col), data = crime_numeric, family = binomial)
summary(log_model)

# Predict probabilities for first 5 rows
predicted_log <- predict(log_model, newdata = crime_numeric[1:5, ], type = "response")
print("Predicted probabilities (first 5 rows) from Logistic Regression:")
print(predicted_log)

# ------------------------------
# 8. Plot Linear Regression
# ------------------------------
ggplot(crime_numeric, aes_string(x = x_col, y = y_col)) +
  geom_point(size = 2, colour = "blue") +
  geom_smooth(method = "lm", colour = "red") +
  ggtitle(paste("Linear Regression:", y_col, "vs", x_col)) +
  xlab(x_col) + ylab(y_col)

# ------------------------------
# 9. Plot Logistic Regression
# ------------------------------
ggplot(crime_numeric, aes_string(x = x_col, y = "target_binary")) +
  geom_point(size = 2, colour = "darkgreen") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), colour = "purple") +
  ggtitle(paste("Logistic Regression:", "Binary Target vs", x_col)) +
  xlab(x_col) + ylab("Binary Target")
# ------------------------------
# 10. Heatmap of all numeric variables
# ------------------------------

# Base R heatmap
heatmap(crime_cor, 
        main = "Heatmap of Numeric Variables",
        col = heat.colors(20), 
        scale = "column", 
        margins = c(8,8))
