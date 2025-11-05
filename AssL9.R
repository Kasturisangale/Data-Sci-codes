# ================== Required Libraries ==================
required_pkgs <- c("caret","e1071","rpart","rpart.plot","ggplot2","dplyr")
to_install <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install, repos='https://cloud.r-project.org')
lapply(required_pkgs, library, character.only = TRUE)

# ================== Load Dataset ==================
set.seed(123)
dataset <- read.csv("C:/Users/Kasturi/Downloads/crime_safety_dataset.csv", stringsAsFactors = TRUE)

# ================== Data Cleaning ==================
dataset$crime_type <- as.factor(dataset$crime_type)
dataset$city <- as.factor(dataset$city)
dataset$state <- as.factor(dataset$state)
dataset$victim_gender <- as.factor(dataset$victim_gender)
dataset$victim_race <- as.factor(dataset$victim_race)

dataset <- na.omit(dataset)

# ================== Create Target ==================
# For demonstration, classify 'High' crime severity if victim_age > 50
dataset$crime_severity <- as.factor(ifelse(dataset$victim_age > 50, "High", "Low"))

# ================== Train-Test Split ==================
trainIndex <- createDataPartition(dataset$crime_severity, p = 0.7, list = FALSE)
trainData <- dataset[trainIndex, ]
testData <- dataset[-trainIndex, ]

# ================== Train Models ==================
# 1. Decision Tree
model_dt <- train(
  crime_severity ~ crime_type + city + state + victim_age + victim_gender + victim_race,
  data = trainData,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5),
  tuneLength = 5
)

# 2. Logistic Regression
model_glm <- train(
  crime_severity ~ crime_type + city + state + victim_age + victim_gender + victim_race,
  data = trainData,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5)
)

# 3. k-Nearest Neighbors (kNN)
model_knn <- train(
  crime_severity ~ crime_type + city + state + victim_age + victim_gender + victim_race,
  data = trainData,
  method = "knn",
  trControl = trainControl(method = "cv", number = 5),
  tuneLength = 10
)

# ================== Evaluation Function ==================
extract_metrics <- function(model, testData, modelName){
  pred <- predict(model, testData)
  cm <- confusionMatrix(pred, testData$crime_severity, positive="High")
  data.frame(
    Model = modelName,
    Accuracy = round(cm$overall['Accuracy'],3),
    Precision = round(cm$byClass['Pos Pred Value'],3),
    Recall = round(cm$byClass['Sensitivity'],3),
    F1_Score = round(cm$byClass['F1'],3)
  )
}

# ================== Compare Models ==================
metrics_dt <- extract_metrics(model_dt, testData, "Decision Tree")
metrics_glm <- extract_metrics(model_glm, testData, "Logistic Regression")
metrics_knn <- extract_metrics(model_knn, testData, "kNN")

performance_table <- rbind(metrics_dt, metrics_glm, metrics_knn)
cat("==============================================\n")
cat("--- Comparative Model Performance Metrics ---\n")
cat("==============================================\n")
print(performance_table)

# ================== Visualization ==================
cat("\n--- Decision Tree Structure ---\n")
rpart.plot(model_dt$finalModel, main = "Decision Tree: Crime Severity")

# Decision boundaries plot (victim_age vs city index, as numeric)
dataset$city_index <- as.numeric(dataset$city)
grid <- expand.grid(
  victim_age = seq(min(dataset$victim_age), max(dataset$victim_age), length.out = 100),
  city_index = seq(min(dataset$city_index), max(dataset$city_index), length.out = 100)
)
grid$crime_type <- levels(dataset$crime_type)[1]
grid$state <- levels(dataset$state)[1]
grid$victim_gender <- levels(dataset$victim_gender)[1]
grid$victim_race <- levels(dataset$victim_race)[1]

grid$Prediction <- predict(model_dt, newdata=grid)

ggplot() +
  geom_tile(data = grid, aes(x = city_index, y = victim_age, fill = Prediction), alpha = 0.3) +
  geom_point(data = dataset, aes(x = city_index, y = victim_age, color = crime_severity, shape = crime_severity), size = 3) +
  labs(title = "Decision Tree Decision Boundaries (Victim Age vs City)",
       x = "City (numeric index)", y = "Victim Age") +
  theme_minimal()
