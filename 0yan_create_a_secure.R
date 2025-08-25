# 0yan_create_a_secure.R

# Load necessary libraries
library(caret)
library(randomForest)
library(Metrics)

# Load dataset
data <- read.csv("secure_data.csv")

# Data Preprocessing
set.seed(123)
data$X <- scale(data$X)

# Split data into train and test sets
trainIndex <- createDataPartition(data$Y, p = 0.7, list = FALSE)
trainSet <- data[ trainIndex,]
testSet <- data[-trainIndex,]

# Define Random Forest Model
model <- randomForest(Y ~ ., data = trainSet, 
                      ntree = 500, 
                      mtry = 2, 
                      nodesize = 5)

# Train Model
train_pred <- predict(model, trainSet, type = "class")
test_pred <- predict(model, testSet, type = "class")

# Evaluate Model
train_confusion_matrix <- confusionMatrix(train_pred, trainSet$Y)
test_confusion_matrix <- confusionMatrix(test_pred, testSet$Y)

# Implement Security Measures
# 1. Anomaly Detection
anomaly_score <- predict(model, testSet, type = "prob")
anomaly_threshold <- 0.5
anomaly_pred <- ifelse(anomaly_score > anomaly_threshold, "Anomaly", "Normal")

# 2. Data Encryption
library(sodium)
set.seed(123)
key <- keygen()
encrypted_test_data <- data_encrypt(testSet, key = key)

# 3. Access Control
access_control <- function(user_id, permission_level) {
  if (user_id == "admin" && permission_level == "high") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Test Access Control
user_id <- "admin"
permission_level <- "high"
if (access_control(user_id, permission_level)) {
  cat("Access Granted!")
} else {
  cat("Access Denied!")
}

# Output
cat("Model Evaluation Metrics:\n")
print(train_confusion_matrix)
print(test_confusion_matrix)

cat("\nAnomaly Detection Results:\n")
print(anomaly_pred)

cat("\nEncrypted Test Data:\n")
print(encrypted_test_data)