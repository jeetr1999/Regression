#Importing Data set 
dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[,-1]
#or dataset <- dataset[2:3]

# #Splitting into training and testing set
# library(caTools)
# set.seed(123)
# split <- sample.split(dataset$Salary, SplitRatio = 0.8)
# training_set <- subset(dataset, split == T)
# test_set <- subset(dataset, split == F)
# remove(split)


#Fitting Linear Regression on Dataset
linear.model <- lm(Salary ~ ., data = dataset)
summary(linear.model)

#Building polynomial terms
dataset$level2 <- dataset$Level^2
dataset$level3 <- dataset$Level^3
dataset$level4 <- dataset$Level^4

#Fitting Polynomial Regression on Dataset
polynomial.model <- lm(Salary ~., data = dataset)
summary(polynomial.model)

polynomial.model2 <- lm(Salary ~., data = dataset)
summary(polynomial.model2)

polynomial.model4 <- lm(Salary ~., data = dataset)
summary(polynomial.model4)

#-----------USE THIS STEP TO SEE HOW ACCURATE THE MODEL IS--------------
library(forecast)
accuracy(linear.model$fitted.values, dataset$Salary)
accuracy(polynomial.model$fitted.values, dataset$Salary)
accuracy(polynomial.model2$fitted.values, dataset$Salary)

#Visualizing Linear Regression Model
library(ggplot2)
ggplot() +
  geom_point(aes(dataset$Level, dataset$Salary),
             color = 'red') + 
  geom_line(aes(dataset$Level, predict(linear.model, dataset)),
            color = 'blue') +
  ggtitle('Position Level vs Experience (Linear Model)') + 
  xlab("Position Level") +
  ylab("Salary")


#Visualizing Polynomial Regression Model
ggplot() +
  geom_point(aes(dataset$Level, dataset$Salary),
             color = 'red') + 
  geom_line(aes(dataset$Level, predict(polynomial.model4, dataset)),
            color = 'blue') +
  ggtitle('Position Level vs Experience (Regression Model)') + 
  xlab("Position Level") +
  ylab("Salary")


#Predicting a new result with linear model
y_pred <- predict(linear.model, data.frame(Level = 6.5))
y_pred

#Predicting a new result with polynomial model
poly_pred <- predict(polynomial.model4, data.frame(Level = 6.5,
                                                   level2 = 6.5^2,
                                                   level3 = 6.5^3,
                                                   level4 = 6.5^4))
poly_pred