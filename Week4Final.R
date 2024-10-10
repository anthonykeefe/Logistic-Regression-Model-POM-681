# Data loaded
data <- read.csv("A6DATA.csv")


# Set seed for reproducibility, plit the data into training (50%) and testing (50%)
set.seed(7102)
trainIndex <- sample(1:nrow(data), 0.5 * nrow(data))
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Fit logistic regression model with ALL predictors
model <- glm(V5 ~ V1 + V2 + V3 + V4, data = train_data, family = binomial)

# Summary of the model to take a look at significant variables
summary(model)

# Refined model based on significant variables (V1, V2, V3)
refined_model <- glm(V5 ~ V1 + V2 + V3, data = train_data, family = binomial)

# Summary of refined model
summary(refined_model)

# Predicting on the training set
train_pred <- predict(refined_model, train_data, type = "response")
train_pred_class <- ifelse(train_pred > 0.5, 1, 0)

# Confusion matrix for training data
table(Predicted = train_pred_class, Actual = train_data$V5)

# Predicting on the testing set
test_pred <- predict(refined_model, test_data, type = "response")
test_pred_class <- ifelse(test_pred > 0.5, 1, 0)

# Confusion matrix for testing data
table(Predicted = test_pred_class, Actual = test_data$V5)

# Misclassification error for training data
train_error <- mean(train_pred_class != train_data$V5)

# Misclassification error for testing data
test_error <- mean(test_pred_class != test_data$V5)


# Output misclassification errors
train_error
test_error

# Number of forged and genuine banknote-like specimens in training and testing data:
table(train_data$V5)  # For training data
table(test_data$V5)    # For testing data

#Probability equation below: 
# Extracted coefficients from my model
intercept <- 6.4248   # Intercept (from the model output)
coef_V1 <- -6.8784    # Coefficient for V1
coef_V2 <- -3.5070    # Coefficient for V2
coef_V3 <- -4.4981    # Coefficient for V3

# Logistic regression function
logistic_function <- function(V1, V2, V3) {
  # Logistic regression equation
  linear_combination <- intercept + (coef_V1 * V1) + (coef_V2 * V2) + (coef_V3 * V3)
  probability <- 1 / (1 + exp(-linear_combination))
  return(probability)
}

# Applied Logistic function to my dataset (train_data)
predicted_probabilities <- logistic_function(train_data$V1, train_data$V2, train_data$V3)

# Present the predicted probabilities
head(predicted_probabilities)

