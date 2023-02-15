#install.packages(c("kohonen", "dummies", "ggplot2", "maptools", "sp","reshape2", "rgeos"))
library(kohonen)
# install.packages("caTools")    # For Logistic regression
# install.packages("ROCR")       # For ROC curve to evaluate model

# Loading package
library(caTools)
library(ROCR)
# library(dummies) # The dummies package is no longer available in CRAN
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgeos)
library(MASS)
library(Hmisc)
library(caTools)
library(ROCR)
library(corrplot)
library(tidyverse)




## Data Loading
gt_2011 <- read.csv("gt_2011.csv")
gt_2012 <- read.csv("gt_2012.csv")
gt_2013 <- read.csv("gt_2013.csv")
gt_2014 <- read.csv("gt_2014.csv")
gt_2015 <- read.csv("gt_2015.csv")



training_set <- rbind(gt_2011,gt_2012)
validate_set <- gt_2013
test_set <- rbind(gt_2014,gt_2015)

# Find missing data
which(is.na(training_set))
sum(is.na(training_set))
# No missing Data 

# Perform Feature Scaling ( Make values all between -1 to 1)
training_set[, 1:9] = scale(training_set[, 1:9])
validate_set[, 1:9] = scale(validate_set[, 1:9])
test_set[, 1:9] = scale(test_set[, 1:9])

hist(training_set)

# Log Transformation 
log_training_set <- training_set
log_training_set[, 1:9] = log(log_training_set[, 1:9])
hist(log_training_set)

# Perform standardization ( Makes data fit into a gaussian dist )
standardize = function(x){
  z <- (x - mean(x)) / sd(x)
  return( z)
}
minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
normalisedMydata <- training_set
normalisedMydata[1:9] <- as.data.frame(lapply(training_set[1:9], minMax))

boxplot(normalisedMydata[1:9])
hist(normalisedMydata[1:9])

training_set[1:9] <-
  apply(training_set[1:9], 2, standardize)

training_set <-training_set %>% mutate_if(negate(is.factor), normalize)


training_set

boxplot(training_set[1:9])

hist(training_set[1:9])

describe(training_set)
hist(training_set)
cor(training_set)
corrplot(cor(training_set[-11]), method="circle", addCoef.col = 'black')

boxplot(training_set[9], show.names=TRUE)

#CO
COcorTable = abs(cor(training_set[,-11], y=training_set$CO))
COcorTable = COcorTable[order(COcorTable, decreasing = TRUE),,drop = FALSE]
head(COcorTable,6)



#NOX
NOXcorTable = abs(cor(training_set[,-10], y=training_set$NOX))
NOXcorTable = NOXcorTable[order(NOXcorTable, decreasing = TRUE),,drop = FALSE]
head(NOXcorTable,6)



model <- lm(training_set, data = training_set)
model
feature_selected_training_set <- training_set[c("TIT","TEY","CDP","GTEP","AT","CO","NOX")]
ggplot(feature_selected_training_set, aes(x = TIT, y = CO)) + geom_point() + stat_smooth()


data.graph<-ggplot(traintestset, aes(x=TIT, y=CO))+geom_point()
data.graph

data.graph <- data.graph + geom_smooth(method="lm", col="black")
data.graph 

pred <- predict(model,test_set)
pred 

rmse_val <- sqrt(mean(pred-test_set$CO)^2)
rmse_val

SSE = sum((pred-test_set$CO)^2)
SST = sum((pred-mean(test_set$CO))^2)
r2_test = 1 - SSE/SST
print(r2_test)



summary(model)
confint(model)
