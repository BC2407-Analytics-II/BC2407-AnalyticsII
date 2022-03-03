# ========================================================================================================
# Purpose:      Rscript for Quantile Regression demo.
# Author:       Chew C.H.
# DOC:          15-09-2017
# Topics:       Quantile Regression
# Data Sources: Engel in package quantreg.
#=========================================================================================================

#setwd('D:/Dropbox/Schools/NBS/BC2407 Analytics 2/S4 Quantile Reg')

library(quantreg)
data(engel)

# Export a R dataset as a CSV file. Other software can then use this dataset. 
write.csv(engel, "engel.csv", row.names = F, col.names = T)

# Linear Regression is inadequate -----------------------------------------

summary(engel$income)
fit.lm <- lm(engel$foodexp ~ engel$income)
summary(fit.lm)
plot(engel$income, engel$foodexp, main = 'Regressions on Engel Food Expenditure Data', xlab = 'Household Income', ylab = 'Food Expenditure')
abline(fit.lm,lty=2,col="red")

par(mfrow=c(2,2))
par(mar=c(1,1,1,1))
plot(fit.lm)
par(mfrow=c(1,1))

library(quantreg)
# Fit 50th Percentile Line (i.e. Median)
fit.p.5 <- rq(engel$foodexp ~ engel$income, tau=.5) #tau: percentile level. 0.5 is the 50th percentile (aka median).
abline(fit.p.5, col="blue")

fit.p.5
summary(fit.p.5)
summary(fit.p.5, se = "nid")

# 5th, 10th, 25th, 75th, 90th, 95th percentiles.
taus <- c(.05, .1, .25, .75, .90, .95)

# Plot the 6 percentile grey lines
for( i in 1:length(taus)){
   abline(rq(engel$foodexp~engel$income,tau=taus[i]), col = "grey")
}

# ii. In the RScript qr1, the 6 quantile regression are plotted as 6 grey lines, 
# but the model coefficients (b0, b1) are not shown. 
# Modify the RScript so that the model parameters for the 6 quantile regression models 
# are exhibited in a table. [Hint: Where is the information saved in the R object?]
intercept = data.frame()
coefficient = data.frame()
tau = data.frame()
for( i in 1:length(taus)){
    model = rq(engel$foodexp~engel$income,tau=taus[i])
    intercept = rbind(intercept, model$coefficients[1])
    coefficient = rbind(coefficient, model$coefficients[2])
    tau = rbind(tau, taus[i])
}
colnames(intercept) = 'Intercept'
intercept['Coefficient'] = coefficient
intercept['Percentile'] = tau
intercept

# iii. One student asked if quantile regression is just fitting linear regression 
# on the specific percentile of the data. True/False? 
# Can you answer this from the software output?
for( i in 1:length(taus)){
    print(rq(engel$foodexp~engel$income,tau=taus[i]))
}
# all degrees of freedom is the same

print('
Activity 2
1.See result in Excel File Check Function solutions.xlsx
2.Yes, all data points are used, regardless of tau value.
3.The higher the tau value, the higher the height of the quantile regression line as the total loss will be lower.
4.Tau at middle values require more data points to disambiguate [clear winning model] compared to Tau at extreme values (e.g. 0.1 or 0.9).
')