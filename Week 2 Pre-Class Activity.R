library(data.table)
library(car)
library(rpart)
library(rpart.plot)

## Activity 1
setwd("C:/Users/Alvin Lim/Desktop/NTU Stuff/Analytics II/S2 Review of Basic Analytics")
df1 <- fread('resale-flat-prices-2019.csv', stringsAsFactors = T)

View(df1)

## Create a derived variable "remaining_lease_years" defined as the remaining lease (in years) of the flat as in 2019 and save it as a new column.
df1$remaining_lease_years <- 99-(2019 - df1$lease_commence_date)
View(df1)

## Set the Baseline Reference level for "town" to "Yishun"
df1$town <- factor(df1$town)
levels(df1$town)

df1$town <- relevel(df1$town, ref = "YISHUN")
levels(df1$town)

## Build a Linear Regression model using floor_area_sqm, remaining_lease_years, town, and storey_range to estimate resale_price.
## Not necessary to create train-test split. Focus on the model creation and outputs.
set.seed(2014)
lm1 <- lm(`resale_price` ~ floor_area_sqm + remaining_lease_years + town + storey_range, data = df1)

## What are the model coefficients, R^2 and RMSE?
summary(lm1)

## R^2 = 0.8416
## Adjusted R-square = 0.8413

RMSE <- sqrt(mean(residuals(lm1)^2))  # RMSE on trainset based on m5 model.
summary(abs(residuals(lm1)))  # Check Min Abs Error and Max Abs Error.
RMSE

## RMSE = 61286.96

## Activity 2
df2 <- fread('default.csv', stringsAsFactors = T)
View(df2)

## Build a Logistic Regression model to predict Default status.
##  Not necessary to create train-test split. Focus on the model creation and outputs.
class(df2$Default)
set.seed(2014)
glm1 <- glm(Default ~. , family = binomial, data = df2)

## What are the model coefficients and confusion matrix?
summary(glm1)
## OR.default <- exp(coef(glm1))
## OR.default

## OR.default.CI <- exp(confint(glm1)) 
## OR.default.CI

default1 <- predict(glm1, type = 'response')
default1
threshold <- 0.5
default.Prediction <- ifelse(default1 > threshold, 'Yes', 'No')
default.Confusion.matrix <- table(df2$Default, default.Prediction, deparse.level = 2)
default.Confusion.matrix

## Which cases has P(Default = Yes) > 90%?
df2$default_probability <- default1
df2.1 <- subset(df2, Default == "Yes")
df2.1
subset(df2.1, default_probability > 0.9)
df2[default1>0.9]


## Activity 3
##  Not necessary to create train-test split. Focus on the model creation and outputs.
df3 <- fread('default.csv', stringsAsFactors = T)
View(df3)

## Build an optimal CART model to predict Default status.
CART.default <- rpart(Default ~., data = df3, method = 'class',control = rpart.control(minsplit = 2, cp = 0))

# plots the maximal tree and results.
rpart.plot(CART.default, nn= T, main = "Maximal Tree")

# prints out the pruning sequence and 10-fold CV errors, as a table.
printcp(CART.default)

# Display the pruning sequence and 10-fold CV errors, as a chart.
plotcp(CART.default, main = "Subtrees in default dataset")

## Automated geometric mean finding
CVerror.cap <- CART.default$cptable[which.min(CART.default$cptable[,"xerror"]),"xerror"] + CART.default$cptable[which.min(CART.default$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (CART.default$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(CART.default$cptable[i,1] * CART.default$cptable[i-1,1]), 1)
cart1 <- prune(CART.default, cp = cp.opt)

# Plug optimal cp to prune model
## How many decision rules are in the optimal CART model and what is the confusion matrix?
printcp(cart1, digits = 3)
print(cart1)
set.seed(2014)
rpart.plot(cart1, nn= T, main = "Pruned Tree")

## Variable Importance

cart1$variable.importance

# Scaling Variable Impt so as to rep as percentage impt 
ScaledVariableImpt <- round(100*cart1$variable.importance/sum(cart1$variable.importance),1)
ScaledVariableImpt

summary(cart1)

# Accuracy & Confusion Matrix
cart.predict <- predict(cart1, type = "class")
results <- data.frame(df3$Default, cart.predict)
results
accuracy <- mean(results$df3.Default == results$cart.predict)
accuracy ##0.9741
CART.Confusion.matrix <- table(df3$Default, cart.predict, deparse.level = 2)
CART.Confusion.matrix

## Which cases has P(Default = Yes) > 90%?
probability <- predict(cart1, type = "prob")
probability
class(probability)

## convert matrix & array to data table before allocating
df4 <- as.data.table(probability)
View(df4)
df3$no_probability <- df4$No
df3$yes_probability <- df4$Yes
View(df3)

subset(df3, yes_probability > 0.9)

## No values found for yes_probability > 0.9

