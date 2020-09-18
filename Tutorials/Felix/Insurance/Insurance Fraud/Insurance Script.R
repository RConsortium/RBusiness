############################
# Load Libraries
library(dplyr)
library(readr)
library(randomForest)
library(rfUtilities)
library(rsample)
library(ggplot2)
library(caret)
library(e1071)

############################
#Loading and Discovery

# Load Data
data <- read_csv("Library/Mobile Documents/com~apple~CloudDocs/Education/R:Business/insurance_claims.csv")

# First Look at Data
head(data)

# Check Types
sapply(data, class)

# Summarize Data
summary(data)

# Check for NAs 
data[!complete.cases(data),] #non found

############################
# Data Wrangling and Split

#Data Wrangling
data$fraud_reported[data$fraud_reported == 'Y']  <- 1
data$fraud_reported[data$fraud_reported == 'N']  <- 0
data$fraud_reported <- as.integer(data$fraud_reported)
data$insured_zip <- as.character(data$insured_zip)
data$policy_bind_date <- as.Date(data$policy_bind_date, format = '%m/%d/%y')

# Check for amount of uniqe values
sapply(sapply(data, unique), length)

# Drop obvious colimns
drops <- c('incident_location','policy_bind_date','incident_date','insured_occupation','insured_zip', 'policy_number')
data <- data[ , !(names(data) %in% drops)]

# turn into factors
data <- data %>% mutate_if(is.character, as.factor)

# For reproducability
set.seed(52)

# Split Data
data_split <- initial_split(data, prop = .7)
data_train <- training(data_split)
data_test  <- testing(data_split )

############################
# Basic Implementation & Analysis

# default RF model
rf1 <- randomForest(
  formula = fraud_reported ~ .,
  data    = data_train)

############################
# Check Results

# Model
rf1

# Plot tree
plot(rf1)

# Lowest MSE
which.min(rf1$mse)

# RMSE of this optimal random forest
sqrt(rf1$mse[which.min(rf1$mse)])

#Variable Importance
View(rf1$importance)

############################
# RF Initial Validation

# Split down set further for validation set on training data

# create training and validation data 
data_split <- initial_split(data_train, .8)

# training data
data_train_v2 <- analysis(data_split)

# validation data
data_valid <- assessment(data_split)
x_test <- data_valid[setdiff(names(data_valid),"fraud_reported")]
y_test <- data_valid$fraud_reported

rf_oob_comp <- randomForest(
  formula = fraud_reported ~ .,
  data    = data_train_v2,
  xtest   = x_test,
  ytest   = y_test)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")

############################
# RF Tuning

# names of features
features <- setdiff(names(data_valid),"fraud_reported")

# tune Random Forest
rf2 <- tuneRF(
  x          = data_train[features],
  y          = data_train$fraud_reported,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = TRUE      # to not show real-time progress 
)

# Try best version
rf2 <- randomForest(
  x          = data_train[features],
  y          = data_train$fraud_reported,
  data    = data_train,
  ntree = 500,
  mtry = 15
)

# Check results
rf2

# Plot tree
plot(rf2)

#Variable Importance
View(rf2$importance)

############################
# Predicting

# randomForest
pred_randomForest <- predict(rf2, data_test)
head(pred_randomForest)

# Make pred column
data_test$pred <- predict(rf2, data_test)
data_test$pred_final <- as.integer(ifelse(data_test$pred > 0.3 ,1 ,0))
                          
# Confusion Matrix
confusionMatrix(as.factor(data_test$fraud_reported), as.factor(data_test$pred_final))



