#Get The Dataset
dataset <- read.csv("50_Startups.csv")

#Encoding Categorical Data
dataset$State <- factor(dataset$State,
                        levels = c('New York', 'California', 'Florida'),
                        labels = c(1, 2, 3))

#Splitting The Data into Training Set and Test Set
library(caTools)
split <-  sample.split(dataset$Profit, 0.8)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

#Using BACKWARD ELIMINATION to build optimal models

regressor <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
                dataset)

"Above we considered the whole dataset to get information about which variables
are more statistically significant and which are not"

summary(regressor)

regressor2 <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend,
                dataset)
summary(regressor2)

#Developing Models
regressor.multiple <- lm(Profit ~ ., dataset)
regressor3 <- lm(Profit ~ R.D.Spend + Marketing.Spend, dataset)
regressor.linear <- lm(Profit ~ R.D.Spend, dataset)

#Testing Accuracy of the model
accuracy(regressor.multiple$fitted.values, training_set$Profit)
accuracy(regressor3$fitted.values, training_set$Profit)
accuracy(regressor.linear$fitted.values, training_set$Profit)



#AUTOMATIC BACKWARD ELIMINATION


backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)