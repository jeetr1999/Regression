# RANDOM FOREST REGRESSION

# Importing The Dataset

dataset <- read.csv("Position_Salaries.csv")
dataset <- dataset[2:3]


# Fitting Random Forest Regression on Whole Data Set
library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[1],
                         y = dataset$Salary,
                         ntree = 500)


#Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))


# Visualizing the Decision Tree results 
# (for higher resolution and smoother curve)
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor,newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')
