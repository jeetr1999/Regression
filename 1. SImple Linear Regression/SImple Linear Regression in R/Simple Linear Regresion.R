#Set Working Directory

#-----------------------Import The Dataset-----------------------
dataset <- read.csv("Salary_Data.csv")

#--------------Splitting The Data in Training Set and Testing Set-------------
library(caTools)
set.seed(123)
split <- sample.split(dataset$Salary, SplitRatio = 2/3)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

#------------Fitting/Implement Simple Regression on Training Set--------------
regressor <- lm(Salary ~ YearsExperience,
                training_set)
summary(regressor)

#------------------------Predicting The Model on Test Set----------------------
y_pred <- predict(regressor, test_set)

#----------Visualizing The Training Set Results and Test Set Results----------

#Training Set
library(ggplot2)
ggplot() +
  geom_point(aes(training_set$YearsExperience, training_set$Salary),
             color = 'red') + 
  geom_line(aes(training_set$YearsExperience, predict(regressor, training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') + 
  xlab("Years of Experience") +
  ylab("Salary")
 

#Test Set
ggplot() +
  geom_point(aes(test_set$YearsExperience, test_set$Salary),
             color = 'red') + 
  geom_line(aes(training_set$YearsExperience, predict(regressor, training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Test Set)') + 
  xlab("Years of Experience") +
  ylab("Salary")
