#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp","reshape2", "rgeos"))
library(kohonen)
# library(dummies) # The dummies package is no longer available in CRAN
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(MASS)
library(Hmisc)
library(caret)
library(corrplot)

## setwd("C:/Users/meiya/Desktop/Degree/Year 2 Quarter 3/INFO411 - Data Mining and Knowledge Discovery/Group Project/pp_gas_emission")

## Data Loading
gt_2011 <- read.csv("gt_2011.csv") 
gt_2012 <- read.csv("gt_2012.csv") 
gt_2013 <- read.csv("gt_2013.csv") 
gt_2014 <- read.csv("gt_2014.csv") 
gt_2015 <- read.csv("gt_2015.csv") 

training_set <- rbind(gt_2011,gt_2012)
validate_set <- gt_2013
test_set <- rbind(gt_2014,gt_2015)

describe(training_set) 

# Histogram of features 
hist(training_set)

# Box Plot of features before Scaling 
boxplot(training_set[1:9])

# Perform Feature Scaling (Make the std deviation of the data = 1) 
training_set[, 1:9] = scale(training_set[, 1:9]) 
validate_set[, 1:9] = scale(validate_set[, 1:9]) 
test_set[, 1:9] = scale(test_set[, 1:9])

# Box Plot of features after Scaling 
boxplot(training_set[1:9])

#Co relation table for CO
COcorTable = abs(cor(training_set[,-11], y=training_set$CO))
COcorTable = COcorTable[order(COcorTable, decreasing = TRUE),,drop = FALSE]
head(COcorTable,6)

#Co relation table for NOX
NOXcorTable = abs(cor(training_set[,-10], y=training_set$NOX))
NOXcorTable = NOXcorTable[order(NOXcorTable, decreasing = TRUE),,drop = FALSE]
head(NOXcorTable,6)

# Plotting Correlation matrices
corrplot(cor(training_set[,-11]), addCoef.col = "black") # For Corr with COR
corrplot(cor(training_set[,-10]), addCoef.col = "black") # For Corr with NOX


#Interesting feature that is highly correlated to CO and NOX
feature_selected_training_set <- training_set[c("TIT","TEY","CDP","GTEP","AT","CO","NOX")]

# Model Training
# Train the model to predict CO
modelCO <- train(CO ~ TIT + TEY + CDP + GTEP + AT,data = feature_selected_training_set,method = "lm")
modelCO

#Predict CO with modelCO
pred <- predict(modelCO,test_set)
pred

#Calculate the accuracy of modelCO
rmse_val <- sqrt(mean(pred-test_set$CO)^2)
rmse_val
SSE = sum((pred-test_set$CO)^2)
SST = sum((pred-mean(test_set$CO))^2)
r2_test = 1 - SSE/SST
print(r2_test)

# Model Hyper parameters training
## 10-fold CV
# possible values: boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV"
fitControl <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

# Train the model to predict CO with hyperparameters tuned
model.cv <- train(CO ~ TIT + TEY + CDP + GTEP + AT,data = feature_selected_training_set,method = "lm",trControl = fitControl)  
model.cv 

#Predict CO with tuned modelCO
pred <- predict(model.cv,test_set)
pred

#Calculate the accuracy of tuned modelCO
rmse_val <- sqrt(mean(pred-test_set$CO)^2)
rmse_val
SSE = sum((pred-test_set$CO)^2)
SST = sum((pred-mean(test_set$CO))^2)
r2_test = 1 - SSE/SST
print(r2_test)

# Model Training
# Train the model to predict NOX
modelNOX <- train(NOX ~ TIT + TEY + CDP + GTEP + AT,data = feature_selected_training_set,method = "lm")
modelNOX

#Predict NOX with modelNOX
pred <- predict(modelNOX,test_set)
pred

#Calculate the accuracy of modelNOX
rmse_val <- sqrt(mean(pred-test_set$NOX)^2)
rmse_val
SSE = sum((pred-test_set$NOX)^2)
SST = sum((pred-mean(test_set$NOX))^2)
r2_test = 1 - SSE/SST
print(r2_test)

# Model Hyper parameters training
## 10-fold CV
# possible values: boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV"
fitControl <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

# Train the model to predict NOX with hyperparameters tuned
model.cv <- train(NOX ~ TIT + TEY + CDP + GTEP + AT,data = feature_selected_training_set,method = "lm",trControl = fitControl)  
model.cv 

#Predict CO with tuned modelNOX
pred <- predict(model.cv,test_set)
pred

#Calculate the accuracy of tuned modelNOX
rmse_val <- sqrt(mean(pred-test_set$NOX)^2)
rmse_val
SSE = sum((pred-test_set$NOX)^2)
SST = sum((pred-mean(test_set$NOX))^2)
r2_test = 1 - SSE/SST
print(r2_test)