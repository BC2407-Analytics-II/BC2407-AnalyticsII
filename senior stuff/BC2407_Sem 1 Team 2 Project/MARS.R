## Import dataset and factor
indian <- read.csv("indian_7jobs_perfect.csv")
summary(indian)

indian$CollegeTier <- as.factor(indian$CollegeTier)
indian$CollegeCityTier <- as.factor(indian$CollegeCityTier)


## Model: MARS
library(earth)

library(caTools)
set.seed(2018)
train <- sample.split(Y = indian$Designation, SplitRatio = 0.7)
trainset <- subset(indian, train == T)
testset <- subset(indian, train == F)

  # Degree of interactions = 1
  mars1 <- earth(Designation ~ ., degree = 1, nfold= 10, data=trainset, glm=list(family=binomial))
  summary(mars1)
  
  # Degree of interactions = 2
  mars2 <- earth(Designation ~ ., degree = 2, nfold= 10, data=trainset, glm=list(family=binomial))
  summary(mars2)
  
  # Degree of interactions = 3
  mars3 <- earth(Designation ~ ., degree = 3, nfold= 10, data=trainset, glm=list(family=binomial))
  summary(mars3)

  # Degree of interactions = 4
  mars4 <- earth(Designation ~ ., degree = 4, nfold= 10, data=trainset, glm=list(family=binomial))
  summary(mars4)
  
## Test accuracy
  predicted_MARS <- predict(mars1, newdata=testset, type='class')
  round(mean(predicted_MARS != testset$Designation),3) # Misclassification Rate = 18.1%
  
  predicted_MARS <- predict(mars2, newdata=testset, type='class')
  round(mean(predicted_MARS != testset$Designation),3) # Misclassification Rate = 19.4%
  
  predicted_MARS <- predict(mars3, newdata=testset, type='class')
  round(mean(predicted_MARS != testset$Designation),3) # Misclassification Rate = 19.4%
  
  predicted_MARS <- predict(mars4, newdata=testset, type='class')
  round(mean(predicted_MARS != testset$Designation),3) # Misclassification Rate = 19.4%


## Important Variables
evimp(mars1) # nueroticism, extraversion, conscientiousness, openess_to_experience, agreeableness
evimp(mars2) # Same ^
evimp(mars3) # Same ^
evimp(mars4) # Same ^


