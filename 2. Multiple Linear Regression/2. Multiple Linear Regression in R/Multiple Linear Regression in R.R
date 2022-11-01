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

#Fitting Multiple Linear Regression on Training Set
regressor <- lm(Profit ~ ., 
                data = training_set)

summary(regressor)

"The significance level column tells us about how significant impact the
independent variable is over the depedent variable - the *** column"
"And the lower the p-value is, the higher the independent variable is significant"
"And generally a good threshold to use for p-value is 5%"
"If the p-value of an independent variable is less than 5%, it is more significant,
and if it is more than that, it is less significant"
"Significant level intepretation -
Between 0 and 0.1% - super significant
Between 0.1 and 1% - highly significant
Between 1% and 5% - significant
Between 5% and 10% - borderline significant
More than 10% - not significant at all"

"So in our implementation we can see that R&D Spend has a huge impact on profit"

simpleregressor <- lm(Profit ~ R.D.Spend, data = training_set)
summary(simpleregressor)


#Predicting The Test Set Results
y_pred <- predict(regressor, test_set)
y_pred
  
y_pred2 <- predict(simpleregressor, test_set)
y_pred2

#Standard Error
y_pred - test_set$Profit
y_pred2 - test_set$Profit

regressor$residuals


#Personal shit
regressor3 <- lm(Profit ~., training_set)
predict3 <- predict(regressor3, newdata = test_set)
test.res <- data.frame(test_set$Profit, predict3 , residuals = 
                          test_set$Profit - predict3)

summary(test.res)
hist(test.res$residuals)

#USE THIS STEP TO SEE HOW ACCURATE THE MODEL IS
accuracy(regressor3$fitted.values, training_set$Profit)
