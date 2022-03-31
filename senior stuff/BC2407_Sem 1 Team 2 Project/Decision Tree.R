## Import dataset and factor
indian <- read.csv("indian_7jobs_perfect.csv")
summary(indian)

indian$CollegeTier <- as.factor(indian$CollegeTier)
indian$CollegeCityTier <- as.factor(indian$CollegeCityTier)


## Model: CART
library(rpart)
library(rpart.plot)
library(randomForest)

library(caTools)
set.seed(2018)
train <- sample.split(Y = indian$Designation, SplitRatio = 0.7)
trainset <- subset(indian, train == T)
testset <- subset(indian, train == F)

  # Unpruned Tree
  tree1 <- rpart(Designation ~	., data = trainset, method="class", control=rpart.control(minsplit=73,cp=0)) # Largest minsplit value that minimises MR in tree1 and tree2 combined
  summary(tree1)
  prp(tree1,varlen = 8)

  # Pruned Tree
  cp.opt <- tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"] 
  tree2 <- prune(tree1, cp = cp.opt)
  summary(tree2)
  prp(tree2,varlen = 8)
  
  # Random Forest
  set.seed(2018)
  rf <- randomForest(Designation~., data = trainset, type = class, ntree = 500, importance = TRUE) #mtry, nodesize left untouched.

  # Testing for minsplit
  a <- data.frame(matrix(ncol = 1, nrow = 100))
  for (i in 1:100){
    tree1 <- rpart(Designation ~	., data = trainset, method="class", control=rpart.control(minsplit=i,cp=0))
    predicted_tree <- predict(tree1, testset, type="class")
    a[i,1] <- round(mean(predicted_tree != testset$Designation), 3)
  }
  which(a$matrix.ncol...1..nrow...100. == min(a[,1]))
  
  
## Test Accuracy
  predicted_tree <- predict(tree1, testset, type="class")
  round(mean(predicted_tree != testset$Designation), 3) # Misclassification Rate = 21.8%
  
  predicted_tree <- predict(tree2, testset, type="class")
  round(mean(predicted_tree != testset$Designation), 3) # Misclassification Rate = 20.3%

  predicted_tree <- predict(rf, testset, type="class")
  round(mean(predicted_tree != testset$Designation), 3) # Misclassification Rate = 17.8%

  
## Significant Variables
printcp(tree1) # collegeGPA, Quant, Domain, agreebleness, conscientiousness, extraversion, openess_to_experience, nueroticism
printcp(tree2) # agreeableness, conscientiousness, extraversion, openess_to_experience
round(importance(rf), 2) # MeanDecreaseGini > 50 i.e Drop Gender, CollegeTier, CollegeCityTier
varImpPlot(rf)


