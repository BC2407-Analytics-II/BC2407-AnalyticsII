# Install packages, if you need ----
#install.packages("data.table")
#install.packages("stringr")
#install.packages("tidyverse")
#install.packages("DMwR")

# Libraries used ----
library(data.table)
library(stringr)
library(tidyverse)
library(ggplot2)
library(DMwR)

# Functions ----
## Returns the mode of the variable
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Import the data ----
setwd("C:\\Users\\nakur\\Google Drive\\01_work_mains\\NTU\\ntu_y2s1\\BC2406 analytics - visualise n predict\\00 Team Assignment and Project\\ibm hr with more row\\")
ibmData = fread("IBM_HR_Data.csv")
typeof(ibmData) # Check that imported as data table
summary(ibmData)


# Data cleaning: Fix columns, remove duplicate rows, remove test rows, set data types ----
## Fix the col names with spaces
names(ibmData) = str_replace_all(names(ibmData), c(" " = "." , "," = "" ))
names(ibmData)

## Removing duplicate rows
nrow(unique(ibmData)) == nrow(ibmData) # check if EmployeeNumber (their ID) is not duplicated
ibmData = distinct(ibmData)
nrow(unique(ibmData)) == nrow(ibmData) # verify all duplicates have been dropped

## Removing rows relating to Application.ID, EducationField, EmployeeSource = 'Test' and 'Testing' and 'test 456'
nrow(ibmData)
ibmData <- ibmData[!EducationField %like% 'Test']
ibmData <- ibmData[!Employee.Source %like% 'Test']
ibmData <- ibmData[!EmployeeNumber %like% 'Test']
ibmData <- ibmData[!EmployeeNumber %like% 'TEST']
ibmData <- ibmData[!Application.ID %like% 'Test']

## Removing rows with wrong values
ibmData <- ibmData[!Application.ID == '?????']
nrow(ibmData)

## Remove useless cols
#
# EmployeeNumber,EducationField,EmployeeCount,
# Application.ID,StandardHours,Employee.Source,Over18
#
ibmData = subset(ibmData, select=-c(EmployeeNumber,EducationField,EmployeeCount,Application.ID,
                                    StandardHours,Employee.Source,Over18))
summary(ibmData)

# Checking data types
sapply(ibmData, class)

# Setting appropriate data types
ibmData$DistanceFromHome = as.integer(ibmData$DistanceFromHome)
ibmData$HourlyRate = as.integer(ibmData$HourlyRate)
ibmData$JobSatisfaction = as.integer(ibmData$JobSatisfaction)
ibmData$MonthlyIncome = as.integer(ibmData$MonthlyIncome)
ibmData$PercentSalaryHike = as.integer(ibmData$PercentSalaryHike)
sapply(ibmData, class) # verify changes


# Data cleaning: Factorise the categorical variables ----
categoricalVars = c('Attrition','BusinessTravel','Department','Education','EnvironmentSatisfaction','Gender',
                    'JobInvolvement','JobLevel','JobRole','JobSatisfaction','MaritalStatus',
                    'OverTime','PerformanceRating','RelationshipSatisfaction','StockOptionLevel','WorkLifeBalance')
ibmData[,(categoricalVars):=lapply(.SD,factor),.SDcols=categoricalVars]
sapply(ibmData, class)

sapply(ibmData, levels) # check the factor levels

# Data cleaning: Remove logical errors ----
## Checking for years more than expected
ibmData[YearsAtCompany > TotalWorkingYears] # rows which should not be possible
ibmData <- ibmData[!YearsAtCompany > TotalWorkingYears]

ibmData[YearsInCurrentRole > TotalWorkingYears] #zero rows
ibmData[YearsInCurrentRole > YearsAtCompany] #zero rows
ibmData[YearsWithCurrManager > YearsAtCompany] #zero rows

ibmData[TotalWorkingYears > Age-18] # 2792 rows which should not be possible
ibmData <- ibmData[!TotalWorkingYears > Age]

## Checking for rates more than expected
ibmData[HourlyRate > DailyRate] #zero rows
ibmData[DailyRate > MonthlyRate] #zero rows

# Data cleaning: Fill in missing variables ----
summary(ibmData)
## Integer variables mostly have no NAs
## Need to check -> Attrition, BusinessTravel, Department, Gender, JobRole, MaritalStatus, OverTime

## Attrition
ibmData[,.N,by=Attrition]
varMode = getmode(ibmData$Attrition)
varMode
ibmData[, AttritionNew := Attrition] #Creating a new column for cleaned Attrition column
ibmData[Attrition == '', AttritionNew:=varMode]
ibmData[,.N,by=AttritionNew]
ibmData <- ibmData[!AttritionNew == "Termination"] # drop rows where attrition is termination
summary(ibmData$AttritionNew)

ibmData$AttritionNew <- droplevels(ibmData$AttritionNew) # drop unreasonable levels

## BusinessTravel
ibmData[,.N,by=BusinessTravel]
varMode = getmode(ibmData$BusinessTravel)
varMode
ibmData[, BusinessTravelNew := BusinessTravel] #Creating a new column for cleaned BusinessTravel column
ibmData[BusinessTravel == '', BusinessTravelNew:=varMode]
ibmData[,.N,by=BusinessTravelNew]
summary(ibmData$BusinessTravelNew)

ibmData$BusinessTravelNew <- droplevels(ibmData$BusinessTravelNew) # drop unreasonable levels

## DailyRate
summary(ibmData$DailyRate)
ibmData[is.na(DailyRate)]  #Viewing the NA rows
ibmData[, DailyRateNew := DailyRate] #Creating a new column for cleaned DailyRate column
ibmData <- ibmData[is.na(DailyRate), DailyRateNew:=round(mean(ibmData$DailyRate,na.rm=TRUE))]
summary(ibmData$DailyRateNew)

## Department
ibmData[,.N,by=Department]
varMode = getmode(ibmData$Department)
varMode
ibmData[, DepartmentNew := Department] #Creating a new column for cleaned Department column
ibmData[Department == '', DepartmentNew:=varMode]
ibmData[,.N,by=DepartmentNew]
summary(ibmData$DepartmentNew)

ibmData$DepartmentNew <- droplevels(ibmData$DepartmentNew) # drop unreasonable levels

## DistanceFromHome
summary(ibmData$DistanceFromHome)
ibmData[is.na(DistanceFromHome)]  #Viewing the NA rows
ibmData[, DistanceFromHomeNew := DistanceFromHome] #Creating a new column for cleaned DistanceFromHome column
ibmData <- ibmData[is.na(DistanceFromHome), DistanceFromHomeNew:=round(mean(ibmData$DistanceFromHome,na.rm=TRUE))]
summary(ibmData$DistanceFromHomeNew)

## Education
summary(ibmData$Education) 
ibmData[is.na(Education)]  #Viewing the NA rows
ibmData[, EducationNew := Education] #Creating a new column for cleaned Education column
varMode = getmode(ibmData$Education)
varMode
ibmData[is.na(Education), EducationNew:=varMode]
summary(ibmData$EducationNew)

ibmData$EducationNew <- droplevels(ibmData$EducationNew) # drop unreasonable levels


## EnvironmentSatisfaction
summary(ibmData$EnvironmentSatisfaction) 
ibmData[is.na(EnvironmentSatisfaction)]  #Viewing the NA rows
ibmData[, EnvironmentSatisfactionNew := EnvironmentSatisfaction] #Creating a new column for cleaned EnvironmentSatisfaction column
varMode = getmode(ibmData$EnvironmentSatisfaction)
varMode
ibmData[is.na(EnvironmentSatisfaction), EnvironmentSatisfactionNew:=varMode]
ibmData[,.N,by=EnvironmentSatisfactionNew]
summary(ibmData$EnvironmentSatisfactionNew) # Levels = 127249 and 129588 seems unreasonable

ibmData$EnvironmentSatisfactionNew <- droplevels(ibmData$EnvironmentSatisfactionNew) # drop unreasonable levels


## Gender
summary(ibmData$Gender)
ibmData[is.na(Gender)]  #Viewing the NA rows
varMode = getmode(ibmData$Gender)
varMode
ibmData[, GenderNew := Gender] #Creating a new column for cleaned Gender column
ibmData[is.na(ibmData$Gender), GenderNew:=varMode]
summary(ibmData$GenderNew) # Levels = "1", "2" are inconsistent and undeciphered

ibmData$GenderNew <- droplevels(ibmData$GenderNew) # drop inconsistent levels


## HourlyRate
summary(ibmData$HourlyRate)
ibmData[is.na(HourlyRate)]  #Viewing the NA rows
ibmData[, HourlyRateNew := HourlyRate] #Creating a new column for cleaned HourlyRate column
ibmData <- ibmData[is.na(HourlyRate), HourlyRateNew:=mean(ibmData$HourlyRate,na.rm=TRUE)]
summary(ibmData$HourlyRateNew)

## JobInvolvement
summary(ibmData$JobInvolvement)
ibmData[is.na(JobInvolvement)]  #Viewing the NA rows
varMode = getmode(ibmData$JobInvolvement)
varMode
ibmData[, JobInvolvementNew := JobInvolvement] #Creating a new column for cleaned JobInvolvement column
ibmData[is.na(ibmData$JobInvolvement), JobInvolvementNew:=varMode]
summary(ibmData$JobInvolvementNew) # Levels = "47", "54" are unreasonable

ibmData$JobInvolvementNew <- droplevels(ibmData$JobInvolvementNew) # drop unreasonable levels


## JobLevel
summary(ibmData$JobLevel)
ibmData[is.na(JobLevel)]  #Viewing the NA rows
varMode = getmode(ibmData$JobLevel)
varMode
ibmData[, JobLevelNew := JobLevel] #Creating a new column for cleaned JobLevel column
ibmData[is.na(ibmData$JobLevel), JobLevelNew:=varMode]
summary(ibmData$JobLevelNew)

## JobRole
summary(ibmData$JobRole)
ibmData[,.N,by=JobRole]
varMode = getmode(ibmData$JobRole)
varMode
ibmData[, JobRoleNew := JobRole] #Creating a new column for cleaned JobRole column
ibmData[JobRole == "", JobRoleNew:=varMode]
summary(ibmData$JobRoleNew) # Levels = "4", "5" are unreasonable

ibmData$JobRoleNew <- droplevels(ibmData$JobRoleNew) # drop unreasonable levels
levels(ibmData$JobRoleNew)

## Job Satisfaction
summary(ibmData$JobSatisfaction)
varMode = getmode(ibmData$JobSatisfaction)
varMode
ibmData[, JobSatisfactionNew := JobSatisfaction] #Creating a new column for cleaned JobSatisfaction column
ibmData[is.na(ibmData$JobSatisfaction), JobSatisfactionNew:=varMode]
summary(ibmData$JobSatisfactionNew) 

## MaritalStatus
summary(ibmData$MaritalStatus)
ibmData[,.N,by=MaritalStatus]
varMode = getmode(ibmData$MaritalStatus)
varMode
ibmData[, MaritalStatusNew := MaritalStatus] #Creating a new column for cleaned MaritalStatus column
ibmData[MaritalStatus == '', MaritalStatusNew:=varMode]
summary(ibmData$MaritalStatusNew) # Level = 4 is unreasonable

ibmData$MaritalStatusNew <- droplevels(ibmData$MaritalStatusNew) # drop unreasonable levels 


## MonthlyIncome
summary(ibmData$MonthlyIncome)
ibmData[, MonthlyIncomeNew := MonthlyIncome] #Creating a new column for cleaned MonthlyIncome column
ibmData <- ibmData[is.na(MonthlyIncome), MonthlyIncomeNew:=round(mean(ibmData$MonthlyIncome,na.rm=TRUE))]
summary(ibmData$MonthlyIncomeNew)

## MonthlyRate
summary(ibmData$MonthlyRate)
ibmData[is.na(MonthlyRate)]  #Viewing the NA rows
ibmData[, MonthlyRateNew := MonthlyRate] #Creating a new column for cleaned MonthlyRate column
ibmData <- ibmData[is.na(MonthlyRate), MonthlyRateNew:=round(mean(ibmData$MonthlyRate,na.rm=TRUE))]
summary(ibmData$MonthlyRateNew)

## NumCompaniesWorked
summary(ibmData$NumCompaniesWorked)
ibmData[is.na(NumCompaniesWorked)]  #Viewing the NA rows
ibmData[, NumCompaniesWorkedNew := NumCompaniesWorked] #Creating a new column for cleaned NumCompaniesWorked column
ibmData <- ibmData[is.na(NumCompaniesWorked), NumCompaniesWorkedNew:=round(mean(ibmData$NumCompaniesWorked,na.rm=TRUE))]
summary(ibmData$NumCompaniesWorkedNew)

## OverTime
summary(ibmData$OverTime)
ibmData[,.N,by=OverTime]
varMode = getmode(ibmData$OverTime)
varMode
ibmData[, OverTimeNew := OverTime] #Creating a new column for cleaned OverTime column
ibmData[OverTime == '', OverTimeNew:=varMode]
ibmData[,.N,by=OverTimeNew]

summary(ibmData$OverTimeNew) # Level = Y is unreasonable

ibmData$OverTimeNew <- droplevels(ibmData$OverTimeNew) # drop unreasonable levels 

## PercentSalaryHike
summary(ibmData$PercentSalaryHike)
ibmData[is.na(PercentSalaryHike)]  #Viewing the NA rows
ibmData[, PercentSalaryHikeNew := PercentSalaryHike] #Creating a new column for cleaned PercentSalaryHike column
ibmData <- ibmData[is.na(PercentSalaryHike), PercentSalaryHikeNew:=round(mean(ibmData$PercentSalaryHike,na.rm=TRUE))]
summary(ibmData$PercentSalaryHikeNew)

## PerformanceRating
summary(ibmData$PerformanceRating)
ibmData[,.N,by=PerformanceRating]
ibmData[is.na(PerformanceRating)]  #Viewing the NA rows
varMode = getmode(ibmData$PerformanceRating)
varMode
ibmData[, PerformanceRatingNew := PerformanceRating] #Creating a new column for cleaned PerformanceRating column
ibmData[is.na(PerformanceRating), PerformanceRatingNew:=varMode]
ibmData[,.N,by=PerformanceRatingNew]

summary(ibmData$PerformanceRatingNew) # Level = 11, 13 are unreasonable

ibmData$PerformanceRatingNew <- droplevels(ibmData$PerformanceRatingNew) # drop unreasonable levels 

## RelationshipSatisfaction
summary(ibmData$RelationshipSatisfaction)
ibmData[,.N,by=RelationshipSatisfaction]
ibmData[is.na(RelationshipSatisfaction)]  #Viewing the NA rows
varMode = getmode(ibmData$RelationshipSatisfaction)
varMode
ibmData[, RelationshipSatisfactionNew := RelationshipSatisfaction] #Creating a new column for cleaned RelationshipSatisfaction column
ibmData[is.na(RelationshipSatisfaction), RelationshipSatisfactionNew:=varMode]
ibmData[,.N,by=RelationshipSatisfactionNew]
summary(ibmData$RelationshipSatisfactionNew)

## StockOptionLevel
summary(ibmData$StockOptionLevel)
ibmData[,.N,by=StockOptionLevel]
ibmData[is.na(StockOptionLevel)]  #Viewing the NA rows
varMode = getmode(ibmData$StockOptionLevel)
varMode
ibmData[, StockOptionLevelNew := StockOptionLevel] #Creating a new column for cleaned StockOptionLevel column
ibmData[is.na(StockOptionLevel), StockOptionLevelNew:=varMode]
ibmData[,.N,by=StockOptionLevelNew]
summary(ibmData$StockOptionLevelNew)

summary(ibmData$StockOptionLevelNew) # Level = 80 is unreasonable

ibmData$StockOptionLevelNew <- droplevels(ibmData$StockOptionLevelNew) # drop unreasonable levels 


## TrainingTimesLastYear
summary(ibmData$TrainingTimesLastYear)
ibmData[is.na(TrainingTimesLastYear)]  #Viewing the NA rows
ibmData[, TrainingTimesLastYearNew := TrainingTimesLastYear] #Creating a new column for cleaned TrainingTimesLastYear column
ibmData <- ibmData[is.na(TrainingTimesLastYear), TrainingTimesLastYearNew:=round(mean(ibmData$TrainingTimesLastYear,na.rm=TRUE))]
summary(ibmData$TrainingTimesLastYearNew)

## WorkLifeBalance
summary(ibmData$WorkLifeBalance)
ibmData[is.na(WorkLifeBalance)]  #Viewing the NA rows
varMode = getmode(ibmData$WorkLifeBalance)
varMode
ibmData[, WorkLifeBalanceNew := WorkLifeBalance] #Creating a new column for cleaned WorkLifeBalance column
ibmData[is.na(WorkLifeBalance), WorkLifeBalanceNew:=varMode]
ibmData[,.N,by=WorkLifeBalanceNew]
summary(ibmData$WorkLifeBalanceNew)

## YearsInCurrentRole
summary(ibmData$YearsInCurrentRole)
ibmData[is.na(YearsInCurrentRole)]  #Viewing the NA rows
ibmData[, YearsInCurrentRoleNew := YearsInCurrentRole] #Creating a new column for cleaned YearsInCurrentRole column
ibmData <- ibmData[is.na(YearsInCurrentRole), YearsInCurrentRoleNew:=round(mean(ibmData$YearsInCurrentRole,na.rm=TRUE))]
summary(ibmData$YearsInCurrentRoleNew)

## YearsSinceLastPromotion
summary(ibmData$YearsSinceLastPromotion)
ibmData[is.na(YearsSinceLastPromotion)]  #Viewing the NA rows
ibmData[, YearsSinceLastPromotionNew := YearsSinceLastPromotion] #Creating a new column for cleaned YearsSinceLastPromotion column
ibmData <- ibmData[is.na(YearsSinceLastPromotion), YearsSinceLastPromotionNew:=round(mean(ibmData$YearsSinceLastPromotion,na.rm=TRUE))]
summary(ibmData$YearsSinceLastPromotionNew)

## YearsWithCurrManager
summary(ibmData$YearsWithCurrManager)
ibmData[is.na(YearsWithCurrManager)]  #Viewing the NA rows
ibmData[, YearsWithCurrManagerNew := YearsWithCurrManager] #Creating a new column for cleaned YearsWithCurrManager column
ibmData <- ibmData[is.na(YearsWithCurrManager), YearsWithCurrManagerNew:=round(mean(ibmData$YearsSinceLastPromotion,na.rm=TRUE))]
summary(ibmData$YearsWithCurrManagerNew)


# Data cleaning: Repeat checking for logical errors using new columns ----
## Checking for years more than expected
ibmData[YearsAtCompany > TotalWorkingYears] # zero rows

ibmData[YearsInCurrentRoleNew > TotalWorkingYears] # zero rows

ibmData[YearsInCurrentRoleNew > YearsAtCompany] # Rows which should not be possible
ibmData <- ibmData[!YearsInCurrentRoleNew > YearsAtCompany] # Drop rows

ibmData[YearsWithCurrManagerNew > YearsAtCompany] # Rows which should not be possible
ibmData <- ibmData[!YearsWithCurrManagerNew > YearsAtCompany] # Drop rows

ibmData[TotalWorkingYears > Age] # zero rows

## Checking for rates more than expected
ibmData[HourlyRateNew > DailyRateNew] #zero rows
ibmData[DailyRateNew > MonthlyRateNew] #zero rows


# End of data cleaning:
summary(ibmData)
write.csv(ibmData,file="IBM_cleaned.csv")



# Correlation matrix ----
# Libraries used
library(tidyverse)
library(lsr)
library(ggplot2)

# Create subset of cleaned columns only for correlation purposes
ibmData_corr = subset(ibmData, select=c(Age,AttritionNew,BusinessTravelNew,DailyRateNew,DepartmentNew,
                                  DistanceFromHomeNew,EducationNew,EnvironmentSatisfactionNew,GenderNew,HourlyRateNew,
                                  JobInvolvementNew,JobLevelNew,JobRoleNew,JobSatisfactionNew,MaritalStatusNew,
                                  MonthlyIncomeNew,MonthlyRateNew,NumCompaniesWorkedNew,OverTimeNew,PercentSalaryHikeNew,
                                  PerformanceRatingNew,RelationshipSatisfactionNew,StockOptionLevelNew,TotalWorkingYears,
                                  TrainingTimesLastYearNew,WorkLifeBalanceNew,YearsAtCompany,YearsInCurrentRoleNew,
                                  YearsSinceLastPromotionNew,YearsWithCurrManagerNew
                                  ))
summary(ibmData_corr)

# Function 
## returns chi sq p-value & cramer v for each pair of variables
getcorr = function(x,y) {
  tbl = ibmData_corr %>% select(x,y) %>% table()
  chisq_pval = chisq.test(tbl)$p.value
  cramV = round(cramersV(tbl), 4) 
  data.frame(x, y, chisq_pval, cramV) }

# Create unique pair combinations of column names
## sorting will help get a better plot (upper triangular)
ibm_comb = data.frame(t(combn(sort(names(ibmData_corr)), 2)), stringsAsFactors = F)

# Apply function to each variable combination pair
ibm_res = map2_df(ibm_comb$X1, ibm_comb$X2, getcorr)

# Plot correlation matrix
ibm_res %>%
  ggplot(aes(x,y,fill=chisq_pval))+
  geom_tile()+
  geom_text(aes(x,y,label=cramV))+
  scale_fill_gradient(low="red", high="yellow")+
  theme_classic()

# Findings
## Fit: YearsAtCompany has highest associations with DailyRateNew, JobLevelNew, JobRoleNew, MonthlyIncomeNew, MonthlyRateNew, TotalWorkingYears, YearsInCurrentRoleNew, YearsSinceLastPromotionNew, YearsWithCurrManagerNew (>20%)
## Link: JobInvolvementNew has highest associations with Age, HourlyRateNew, DailyRateNew, MonthlyIncomeNew, MonthlyRateNew, PercentSalaryHikeNew, TotalWorkingYears, YearsInCurrRole, YearsWithCurrManagerNew (>10%)
## Sacrifice: JobSatisfactionNew has highest associations with Age, DailyRateNew, HourlyRateNew, MonthlyIncomeNew, MonthlyRateNew, TotalWorkingYears, YearsInCurrentRoleNew (>10%)
## excluded the 3 Y variables

# Create data table for each component ----
fit.dt = subset(ibmData, select=c(DailyRateNew,JobLevelNew,JobRoleNew,MonthlyIncomeNew,MonthlyRateNew,TotalWorkingYears,
                                  YearsInCurrentRoleNew,YearsSinceLastPromotionNew,YearsWithCurrManagerNew,
                                  YearsAtCompany))

link.dt = subset(ibmData, select=c(Age,DailyRateNew,HourlyRateNew,MonthlyIncomeNew,MonthlyRateNew,
                                   PercentSalaryHikeNew,TotalWorkingYears,YearsInCurrentRoleNew,YearsWithCurrManagerNew,
                                   JobInvolvementNew))

sacrifice.dt = subset(ibmData, select=c(Age,DailyRateNew,HourlyRateNew,MonthlyIncomeNew,MonthlyRateNew,TotalWorkingYears,YearsInCurrentRoleNew,
                                        JobSatisfactionNew))

attrition.dt = subset(ibmData, select=c(YearsAtCompany,JobInvolvementNew,JobSatisfactionNew,
                                        AttritionNew))

# Train-Test Split: Fit ----
# Library used
library(caTools)

# Generate a random number sequence that can be reproduced to verify results.
set.seed(2004)

# 70% trainset. Using Y = YearsAtCompany
train <- sample.split(Y=fit.dt$YearsAtCompany, SplitRatio = 0.7)
fit_trainset <- subset(fit.dt, train == T)
fit_testset <- subset(fit.dt, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(fit_trainset$YearsAtCompany)
summary(fit_testset$YearsAtCompany) ## if both summaries are very diff --> test will not be reliable (high RMSE)


# Train-Test Split: Link ----
# Library used
library(caTools)

# Generate a random number sequence that can be reproduced to verify results.
set.seed(2004)

# 70% trainset. Using Y = JobInvolvementNew
train <- sample.split(Y = link.dt$JobInvolvementNew, SplitRatio = 0.7)
link_trainset <- subset(link.dt, train == T)
link_testset <- subset(link.dt, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(link_trainset$JobInvolvementNew)
summary(link_testset$JobInvolvementNew)

# Train-Test Split: Sacrifice ----
# Library used
library(caTools)

# Generate a random number sequence that can be reproduced to verify results.
set.seed(2004)

# 70% trainset. Using Y = JobSatisfactionNew
train <- sample.split(Y = sacrifice.dt$JobSatisfactionNew, SplitRatio = 0.7)
sac_trainset <- subset(sacrifice.dt, train == T)
sac_testset <- subset(sacrifice.dt, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(sac_trainset$JobSatisfactionNew)
summary(sac_testset$JobSatisfactionNew)

# Train-Test Split: Attrition ----
# Library used
library(caTools)

# Generate a random number sequence that can be reproduced to verify results.
set.seed(2004)

# 70% trainset. Using Y = AttritionNew
train <- sample.split(Y = attrition.dt$AttritionNew, SplitRatio = 0.7)
att_trainset <- subset(attrition.dt, train == T)
att_testset <- subset(attrition.dt, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(att_trainset$AttritionNew)
summary(att_testset$AttritionNew)


# Data Exploration: Fit ----
library(ggplot2)

# YearsAtCompany
ggplot(data=fit.dt, aes(x=YearsAtCompany)) + geom_bar() + labs(title="Count of YearsAtCompany") #barplot
summary(fit.dt$YearsAtCompany)

# JoblevelNew
ggplot(data=fit.dt, aes(x=JobLevelNew)) + geom_bar() + labs(title="Count of JobLevelNew") #barplot
summary(fit.dt$JobLevelNew)/sum(summary(fit.dt$JobLevelNew)) #Level1: 37.26%, Level2: 36.67%, etc

# JobLevelNew vs YearsAtCompany
ggplot(data=fit.dt, aes(x=JobLevelNew, y=YearsAtCompany)) + geom_boxplot() + labs(title="JobLevelNew vs YearsAtCompany")+ stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") # boxplot,
aggregate(YearsAtCompany ~  JobLevelNew, fit.dt, mean) #Avg @ Lvl1: 3.99 -> Lvl5: 14.55
ggplot(data=fit.dt, aes(x=JobLevelNew, y=YearsAtCompany)) + geom_violin() + labs(title="JobLevelNew vs YearsAtCompany") # violinplot

# MonthlyIncomeNew
ggplot(data=fit.dt, aes(x=MonthlyIncomeNew)) + geom_histogram() + labs(title="Count of MonthlyIncomeNew") #Histogram
summary(fit.dt$MonthlyIncomeNew)

# MonthlyIncomeNew vs YearsAtCompany
ggplot(data=fit.dt, aes(x=MonthlyIncomeNew, y=YearsAtCompany)) + geom_point() + labs(title="MonthlyIncomeNew vs YearsAtCompany") # scatterplot
ggplot(data=fit.dt, aes(x=MonthlyIncomeNew, y=YearsAtCompany)) + geom_smooth() + labs(title="MonthlyIncomeNew vs YearsAtCompany") # smooth

# JobRoleNew
ggplot(data=fit.dt, aes(x=JobRoleNew)) + geom_bar() + labs(title="Count of JobRoleNew") #barplot
summary(fit.dt$JobRoleNew)/sum(summary(fit.dt$JobRoleNew)) #Leadership roles:44% , Non-leadership roles:56%

# JobRoleNew vs YearsAtCompany
ggplot(data=fit.dt, aes(x=JobRoleNew, y=YearsAtCompany)) + geom_boxplot() + labs(title="JobRoleNew vs YearsAtCompany")+ stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red") # boxplot
aggregate(YearsAtCompany ~  JobRoleNew, fit.dt, mean) # Manager avg: 14.12yrs, Sales Rep avg: 3.02yrs

# DailyRateNew vs YearsAtCompany
ggplot(data=fit.dt, aes(x=DailyRateNew, y=YearsAtCompany)) + geom_point() + labs(title="DailyRateNew vs YearsAtCompany") # scatterplot

# MonthlyRateNew vs YearsAtCompany
ggplot(data=fit.dt, aes(x=MonthlyRateNew, y=YearsAtCompany)) + geom_point() + labs(title="MonthlyRateNew vs YearsAtCompany") # scatterplot

# TotalWorkingYears vs YearsAtCompany
ggplot(data=fit.dt, aes(x=TotalWorkingYears, y=YearsAtCompany)) + geom_point() + labs(title="TotalWorkingYears vs YearsAtCompany") # scatterplot
# can use to verify logical cleaning

# YearsInCurrentRoleNew vs YearsAtCompany
ggplot(data=fit.dt, aes(x=YearsInCurrentRoleNew, y=YearsAtCompany)) + geom_point() + labs(title="YearsInCurrentRoleNew vs YearsAtCompany") # scatterplot
# can use to verify logical cleaning

# YearsSinceLastPromotionNew vs YearsAtCompany
ggplot(data=fit.dt, aes(x=YearsSinceLastPromotionNew, y=YearsAtCompany)) + geom_point() + labs(title="YearsSinceLastPromotionNew vs YearsAtCompany") # scatterplot
# can use to verify logical cleaning

# YearsWithCurrManagerNew vs YearsAtCompany
ggplot(data=fit.dt, aes(x=YearsWithCurrManagerNew, y=YearsAtCompany)) + geom_point() + labs(title="YearsWithCurrManagerNew vs YearsAtCompany") # scatterplot
# can use to verify logical cleaning

# Data Exploration: Link ----
# JobInvolvementNew
ggplot(data=link.dt, aes(x=JobInvolvementNew, fill=JobInvolvementNew)) + 
  geom_bar(aes(position = "fill"), color = 'Black')+ ggtitle("Count of JobInvolvementNew")+
  labs(x="JobInvolvementNew", y="Count")

# Age vs JobInvolvementNew
ggplot(data=link.dt, aes(x=Age, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="Age", color="JobInvolvementNew")

# DailyRateNew vs JobInvolvementNew
ggplot(data=link.dt, aes(x=DailyRateNew, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="DailyRateNew", color="JobInvolvementNew")

# HourlyRateNew vs JobInvolvementNew
ggplot(data=link.dt, aes(x=HourlyRateNew, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="HourlyRateNew", color="JobInvolvementNew")

# MonthlyIncomeNew vs JobInvolvementNew
ggplot(data=link.dt, aes(x=MonthlyIncomeNew, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="MonthlyIncomeNew", color="JobInvolvementNew")
ggplot(data=link.dt, aes(x=MonthlyIncomeNew, y=JobInvolvementNew))+
  geom_boxplot(fill="white", alpha=0.5, outlier.colour = "red", outlier.shape = 1)+
  labs(x="MonthlyIncomeNew", y='JobInvolvementNew')+ggtitle("MonthlyIncomeNew vs JobInvolvementNew")

# MonthlyRateNew vs JobInvolvementNew
ggplot(data=link.dt, aes(x=MonthlyRateNew, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="MonthlyRateNew", color="JobInvolvementNew")

# PercentSalaryHikeNew vs JobInvolvementNew
ggplot(data=link.dt, aes(x=PercentSalaryHikeNew, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="PercentSalaryHikeNew", color="JobInvolvementNew")
ggplot(data=link.dt, aes(x=PercentSalaryHikeNew, y=JobInvolvementNew))+
  geom_boxplot(fill="white", alpha=0.5, outlier.colour = "red", outlier.shape = 1)+
  labs(x="PercentSalaryHikeNew", y='JobInvolvementNew')+ggtitle("PercentSalaryHikeNew vs JobInvolvementNew")

# TotalWorkingYears vs JobInvolvementNew
ggplot(data=link.dt, aes(x=TotalWorkingYears, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="TotalWorkingYears", color="JobInvolvementNew")

# YearsInCurrentRoleNew vs JobInvolvementNew
ggplot(data=link.dt, aes(x=YearsInCurrentRoleNew, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="YearsInCurrentRoleNew", color="JobInvolvementNew")

# YearsWithCurrManagerNew vs JobInvolvementNew
ggplot(data=link.dt, aes(x=YearsWithCurrManagerNew, color=JobInvolvementNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="YearsWithCurrManagerNew", color="JobInvolvementNew")

# Data Exploration: Sacrifice ----
# JobSatisfactionNew
ggplot(data=sacrifice.dt, aes(x=JobSatisfactionNew, fill=JobSatisfactionNew)) + 
  geom_bar(aes(position = "fill"), color = 'Black')+ ggtitle("Count of JobSatisfactionNew")+
  labs(x="JobSatisfactionNew", y="Count")

# Age vs JobSatisfactionNew
ggplot(data=sacrifice.dt, aes(x=Age, color=JobSatisfactionNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="Age", color="JobSatisfactionNew")

# DailyRateNew vs JobSatisfactionNew
ggplot(data=sacrifice.dt, aes(x=DailyRateNew, color=JobSatisfactionNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="DailyRateNew", color="JobSatisfactionNew")

# HourlyRateNew vs JobSatisfactionNew
ggplot(data=sacrifice.dt, aes(x=HourlyRateNew, color=JobSatisfactionNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="HourlyRateNew", color="JobSatisfactionNew")

# MonthlyIncomeNew vs JobSatisfactionNew
ggplot(data=sacrifice.dt, aes(x=MonthlyIncomeNew, color=JobSatisfactionNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="MonthlyIncomeNew", color="JobSatisfactionNew")
ggplot(data=sacrifice.dt, aes(x=MonthlyIncomeNew, y=JobSatisfactionNew))+
  geom_boxplot(fill="white", alpha=0.5, outlier.colour = "red", outlier.shape = 1)+
  labs(x="MonthlyIncomeNew", y='JobSatisfactionNew')+ggtitle("MonthlyIncomeNew vs JobSatisfactionNew")

# MonthlyRateNew vs JobSatisfactionNew
ggplot(data=sacrifice.dt, aes(x=MonthlyRateNew, color=JobSatisfactionNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="MonthlyRateNew", color="JobSatisfactionNew")

# TotalWorkingYears vs JobSatisfactionNew
ggplot(data=sacrifice.dt, aes(x=TotalWorkingYears, color=JobSatisfactionNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="TotalWorkingYears", color="JobSatisfactionNew")
ggplot(data=sacrifice.dt, aes(x=TotalWorkingYears, y=JobSatisfactionNew))+
  geom_boxplot(fill="white", alpha=0.5, outlier.colour = "red", outlier.shape = 1)+
  labs(x="TotalWorkingYears", y='JobSatisfactionNew')+ggtitle("TotalWorkingYears vs JobSatisfactionNew")

# YearsInCurrentRoleNew vs JobSatisfactionNew
ggplot(data=sacrifice.dt, aes(x=YearsInCurrentRoleNew, color=JobSatisfactionNew))+
  geom_density(fill="white", alpha=0.4, position="identity")+
  labs(x="YearsInCurrentRoleNew", color="JobSatisfactionNew")

# Data Exploration: Attrition ----
# AttritionNew
att.gg <- ggplot(data=attrition.dt)
att.gg + 
  geom_bar(aes(AttritionNew, fill=AttritionNew)) + 
  labs(title="Count of AttritionNew") #minority class possible problem with training

# AttritionNew vs JobSatisfactionNew
att.gg +
  geom_bar(aes(x=JobSatisfactionNew)) + 
  labs(title="Count of AttritionNew vs JobSatisfactionNew") + 
  facet_grid(rows=vars(AttritionNew))
table(attrition.dt$AttritionNew, attrition.dt$JobSatisfactionNew)

# AttritionNew vs JobInvolvementNew
att.gg + 
  geom_bar(aes(x=JobInvolvementNew)) + 
  labs(title="Count of AttritionNew vs JobInvolvementNew") + 
  facet_grid(rows=vars(AttritionNew))

# AttritionNew vs YearsAtCompany
att.gg+
  geom_density(aes(x=YearsAtCompany, color=AttritionNew),fill="white", alpha=0.4, position="identity")+
  labs(x="YearsAtCompany", color="AttritionNew", title="AttritionNew vs Years At Company")

# JobInvolvementNew vs JobSatisfactionNew
table(attrition.dt$JobInvolvementNew,attrition.dt$JobSatisfactionNew)
table(attrition.dt$JobInvolvementNew,attrition.dt$JobSatisfactionNew)*100/nrow(attrition.dt)
ggplot(data=attrition.dt, aes(x=JobSatisfactionNew, y=JobInvolvementNew)) + 
  geom_count() + 
  labs(title="Years at Company vs JobSatisfactionNew")

# JobInvolvementNew vs JobSatisfactionNew vs Mean YearsAtCompany
attrition.dt[,YearsAtCompany_mean :=lapply(.SD,mean), .SDcols="YearsAtCompany",by=.(JobInvolvementNew,JobSatisfactionNew)]

ggplot(data=attrition.dt, aes(x=JobSatisfactionNew, y=JobInvolvementNew)) +
  geom_raster(aes(x=JobInvolvementNew,y=JobSatisfactionNew,fill=YearsAtCompany_mean)) +
  labs(title="Mean of Years At Company vs Job Satisfaction & Involvement",fill="Mean")

# JobInvolvementNew vs JobSatisfactionNew vs Median YearsAtCompany
attrition.dt[,YearsAtCompany_median :=lapply(.SD,median), .SDcols="YearsAtCompany",by=.(JobInvolvementNew,JobSatisfactionNew)]

ggplot(data=attrition.dt, aes(x=JobSatisfactionNew, y=JobInvolvementNew)) +
  geom_raster(aes(x=JobInvolvementNew,y=JobSatisfactionNew,fill=YearsAtCompany_median)) +
  labs(title="Median of Years At Company vs Job Satisfaction & Involvement",fill="Median")

# JobInvolvementNew vs JobSatisfactionNew where AttritionNew == "Voluntary Resignation"
att.VR <- attrition.dt[AttritionNew=="Voluntary Resignation",][,VR_count :=.N, by=.(JobInvolvementNew,JobSatisfactionNew)]
summary(att.VR)
ggplot(data=att.VR, aes(x=JobSatisfactionNew, y=JobInvolvementNew)) +
  geom_raster(aes(x=JobInvolvementNew,y=JobSatisfactionNew, fill=VR_count))

att.Emp <- attrition.dt[AttritionNew=="Current employee",][,CE_count :=.N, by=.(JobInvolvementNew,JobSatisfactionNew)]
ggplot(data=att.Emp, aes(x=JobSatisfactionNew, y=JobInvolvementNew)) +
  geom_raster(aes(x=JobInvolvementNew,y=JobSatisfactionNew, fill=CE_count))


# Linear model: Fit ----
# Libraries used
library(car)

# Fit.lm1 with all x variable
fit.lm1 <- lm(YearsAtCompany ~ ., data = fit_trainset)
summary(fit.lm1)
## The linear model suggests that all x variables are statistically significant
## Adjusted R^2 = 78.01% of data can be explained by linear model --> model is good fit
## Coefficients of DailyRateNew, JobLevelNew, JobRoleNew, MonthlyIncomeNew, MonthlyRateNew are negative --> no sense -> check VIF

# Multi-collinearity test
vif(fit.lm1)
## VIF for JobLevelNew, JobRoleNew, MonthlyIncomeNew > 10
## remove variable that has highest vif (JobLevelNew) then rerun vif again

# Fit.lm2 with all less JobLevelNew
fit.lm2 <- lm(YearsAtCompany ~ .-JobLevelNew, data = fit_trainset)
summary(fit.lm2)
vif(fit.lm2)
## VIF now all < 10
## Adjusted R^2 = 77.52%

# Diagnostic checks
par(mfrow = c(2,2))
par(mar = c(1,1,1,1))
plot(fit.lm2)
par(mfrow = c(1,1)) 
## TL: Observable linear association
## TR: Not in line with normal distribution with mean 0
## BL: Standard deviation --> quite constant
## BR: Influential outlier --> none

# Residual check
d_lm2 <- density(fit.lm2[['residuals']])
plot(d_lm2, main='Residual KDE Plot', xlab = 'Residual value')
## right skewed distribution --> try log(Y+1)

# Fit.lm3 where Y = log(YearsAtCompany+1)
fit.lm3 <- lm(log(YearsAtCompany+1) ~ .-JobLevelNew, data = fit_trainset)
summary(fit.lm3)
## The linear model suggests that DailyRateNew & MonthlyRateNew are statistically insignificant --> remove
## Adjusted R^2 = 77.8% of data can be explained by linear model

# Fit.lm4 without insignificant x variables
fit.lm4 <- lm(log(YearsAtCompany+1) ~ .-JobLevelNew-DailyRateNew-MonthlyRateNew, data = fit_trainset)
summary(fit.lm4)
## Adjusted R^2 = 77.8% of data can be explained by linear model

# Multi-collinearity test
vif(fit.lm4)
## VIF all < 10

# Diagnostic checks
par(mfrow = c(2,2))
plot(fit.lm4)
par(mfrow = c(1,1))
## TL: Observable linear association
## TR: Resembles normal distribution with mean 0
## BL: Standard deviation --> very constant
## BR: Influential outlier --> none

# Residual check
d_lm4 <- density(fit.lm4[['residuals']])
plot(d_lm4, main='Residual KDE Plot', xlab = 'Residual value')
## more like a standard distribution

# RMSE for trainset
RMSE.fit_trainset <- sqrt(mean(residuals(fit.lm4)^2)) 
summary(abs(residuals(fit.lm4)))  # Min abs error = 0.0000086, Max abs error = 1.3340123

# Test fit.lm4 model
lm.predict <- predict(fit.lm4, newdata = fit_testset)
fit_testseterror <- log(fit_testset$YearsAtCompany+1) - lm.predict

# RMSE for testset
RMSE.fit_testset <- sqrt(mean(fit_testseterror^2))
summary(abs(fit_testseterror)) # Min abs error = 0.0000434, Max abs error = 1.3340123 (same)
RMSE.fit_trainset # 0.355608
RMSE.fit_testset # 0.3522699 (slightly better)

# CART model: Fit ----
# Libraries used
library(data.table)
library(rpart)
library(rpart.plot) 

# Generate a random number sequence that can be reproduced to verify results.
set.seed(2004)

# Maximal tree
fit.cart1 <- rpart(YearsAtCompany ~ ., data = fit_trainset, method = 'anova',
                    control = rpart.control(minsplit = 12, cp = 0))
rpart.plot(fit.cart1, nn= T, main = "Maximal Tree in fit_trainset")
print(fit.cart1)

# Shows pruning sequence
printcp(fit.cart1)
plotcp(fit.cart1, main = "Subtrees in fit_trainset") ## too many trees --> use while loop to find cp.opt

# While loop to extract optimal cp to prune
## Compute CVerror.cap = min CVerror + 1SE in maximal tree fit.cart1
fit_CVerror.cap <- fit.cart1$cptable[which.min(fit.cart1$cptable[,"xerror"]), "xerror"] + fit.cart1$cptable[which.min(fit.cart1$cptable[,"xerror"]), "xstd"]
## Find the optimal CP
i <- 1; j<- 4
while (fit.cart1$cptable[i,j] > fit_CVerror.cap) {
  i <- i + 1
} ## i=394 --> optimal tree
## Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
fit_cp.opt = ifelse(i > 1, sqrt(fit.cart1$cptable[i,1] * fit.cart1$cptable[i-1,1]), 1)

# Prune fit_cp.opt
fit.cart2 <- prune(fit.cart1, cp = fit_cp.opt)
printcp(fit.cart2)
rpart.plot(fit.cart2, nn= T, main = "Pruned Tree with cp = 8.5194e-06")
## Root node error: 578403/16197 = 35.71
## Cart2 trainset MSE = ?
## Cart2 testset MSE = ?

# Variable importance
fit.cart2$variable.importance
## YearsInCurrentRoleNew has the highest importance, followed by YearsWithCurrManagerNew

# Logistic model: Link ----
# Libraries used
library(nnet)

# Check and set baseline for JobInvolvementNew to be 2 (Neutral)
levels(link_trainset$JobInvolvementNew) # baseline is 1
link_trainset$JobInvolvementNew <- relevel(link_trainset$JobInvolvementNew, ref=2)
levels(link_trainset$JobInvolvementNew)

# Link.log1 with 4 categorical Y variables
link.log1 <- multinom(JobInvolvementNew ~ ., data = link_trainset)
summary(link.log1)

# Odds ratio
OR.link_log1 <- exp(coef(link.log1))
OR.link_log1
OR.CI.link_log1 <- exp(confint(link.log1))
OR.CI.link_log1

# Z-test
z <- summary(link.log1)$coefficients/summary(link.log1)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue
## Only PercentSalaryHikeNew, YearsInCurrentRoleNew, YearsWithCurrManagerNew are statistically significant (p value < 0.05)

# Link.log2 without insignificant x variables
link.log2 <- multinom(JobInvolvementNew ~ PercentSalaryHikeNew+YearsInCurrentRoleNew+YearsWithCurrManagerNew, data = link_trainset)
summary(link.log2)

# Test link.log2 model
log.predict_prob <- predict(link.log2, newdata = link_testset, type = 'prob')
log.predict <- predict(link.log2, newdata = link_testset, type = 'class')
table(link_testset$JobInvolvementNew, log.predict, deparse.level = 2)

# Overall Accuracy
mean(log.predict == link_testset$JobInvolvementNew)
## 0.5922512

# CART model: Link ----
# Libraries used
library(data.table)
library(rpart)
library(rpart.plot) 

# Generate a random number sequence that can be reproduced to verify results.
set.seed(2004)

# Maximal tree
link.cart1 <- rpart(JobInvolvementNew ~ ., data = link_trainset, method = 'class',
            control = rpart.control(minsplit = 12, cp = 0))
rpart.plot(link.cart1, nn= T, main = "Maximal Tree in link_trainset")
print(link.cart1)

# Shows pruning sequence
printcp(link.cart1)
plotcp(link.cart1, main = "Subtrees in link_trainset") ## too many trees --> use while loop to find cp.opt

# While loop to extract optimal cp to prune
## Compute CVerror.cap = min CVerror + 1SE in maximal tree link.cart1
link_CVerror.cap <- link.cart1$cptable[which.min(link.cart1$cptable[,"xerror"]), "xerror"] + link.cart1$cptable[which.min(link.cart1$cptable[,"xerror"]), "xstd"]
## Find the optimal CP
i <- 1; j<- 4
while (link.cart1$cptable[i,j] > link_CVerror.cap) {
  i <- i + 1
} ## i=47 --> optimal tree
## Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
link_cp.opt = ifelse(i > 1, sqrt(link.cart1$cptable[i,1] * link.cart1$cptable[i-1,1]), 1)

# Prune link_cp.opt
link.cart2 <- prune(link.cart1, cp = link_cp.opt)
printcp(link.cart2) 
plotcp(link.cart2, main = "Subtrees in pruned link_trainset")
rpart.plot(link.cart2, nn= T, main = "Pruned Tree with cp = 0.000331802") ## left 633 split & 634 terminal nodes
## Root node error: 6603/16198 = 0.40764
## Cart2 trainset misclassification error = 0.073300 * 0.40764
## Cart2 testset misclassification error = 0.13100 * 0.40764


# Test link.cart2 model
cart.predict <- predict(link.cart2, newdata = link_testset, type = "class")
table(link_testset$JobInvolvementNew, cart.predict, deparse.level = 2)

# Overall accuracy
mean(cart.predict == link_testset$JobInvolvementNew)
## 0.9461328

# Variable importance
link.cart2$variable.importance
## MonthlyRateNew has the highest importance, followed by MonthlyIncomeNew

# Logistic model: Sacrifice ----
# Libraries used
library(nnet)

# Check and set baseline for JobSatisfactionNew to be 2 (Neutral)
levels(sac_trainset$JobSatisfactionNew) # baseline is 1
sac_trainset$JobSatisfactionNew <- relevel(sac_trainset$JobSatisfactionNew, ref=2)
levels(sac_trainset$JobSatisfactionNew)

# Sac.log1 with 4 categorical Y variables
sac.log1 <- multinom(JobSatisfactionNew ~ ., data = sac_trainset)
summary(sac.log1)

# Odds ratio
OR.sac_log1 <- exp(coef(sac.log1))
OR.sac_log1
OR.CI.sac_log1 <- exp(confint(sac.log1))
OR.CI.sac_log1

# Z-test
z <- summary(sac.log1)$coefficients/summary(sac.log1)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue
## Only Age, HourlyRateNew are statistically significant (p value < 0.05)

# Sac.log2 without insignificant x variables
sac.log2 <- multinom(JobSatisfactionNew ~ Age+HourlyRateNew, data = sac_trainset)
summary(sac.log2)

# Test sac.log2 model
log.predict_prob <- predict(sac.log2, newdata = sac_testset, type = 'prob')
log.predict <- predict(sac.log2, newdata = sac_testset, type = 'class')
table(sac_testset$JobSatisfactionNew, log.predict, deparse.level = 2)

# Overall Accuracy
mean(log.predict == sac_testset$JobSatisfactionNew)
## 0.3151375


# CART model: Sacrifice ----
# Libraries used
library(data.table)
library(rpart)
library(rpart.plot) 

# Generate a random number sequence that can be reproduced to verify results.
set.seed(2004)

# Maximal tree
sac.cart1 <- rpart(JobSatisfactionNew ~ ., data = sac_trainset, method = 'class',
                    control = rpart.control(minsplit = 12, cp = 0))
rpart.plot(sac.cart1, nn= T, main = "Maximal Tree in sacrifice_trainset")
print(sac.cart1)

# Shows pruning sequence
printcp(sac.cart1)
plotcp(sac.cart1, main = "Subtrees in sacrifice_trainset") ## too many trees --> use while loop to find cp.opt

# While loop to extract optimal cp to prune
## Compute CVerror.cap = min CVerror + 1SE in maximal tree sac.cart1
sac_CVerror.cap <- sac.cart1$cptable[which.min(sac.cart1$cptable[,"xerror"]), "xerror"] + sac.cart1$cptable[which.min(sac.cart1$cptable[,"xerror"]), "xstd"]
## Find the optimal CP
i <- 1; j<- 4
while (sac.cart1$cptable[i,j] > sac_CVerror.cap) {
  i <- i + 1
} ## i=63 --> optimal tree
## Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
sac_cp.opt = ifelse(i > 1, sqrt(sac.cart1$cptable[i,1] * sac.cart1$cptable[i-1,1]), 1)

# Prune sac_cp.opt
sac.cart2 <- prune(sac.cart1, cp = sac_cp.opt)
printcp(sac.cart2) 
plotcp(sac.cart2, main = "Subtrees in pruned sac_trainset")
rpart.plot(sac.cart2, nn= T, main = "Pruned Tree with cp = 0.000126790") ## left 824 split & 825 terminal nodes
## Root node error: 11154/16198 = 0.6886
## Cart2 trainset misclassification error = 0.077820 * 0.6886
## Cart2 testset misclassification error = 0.12516 * 0.6886

# Test sac.cart2 model
cart.predict <- predict(sac.cart2, newdata = sac_testset, type = "class")
table(sac_testset$JobSatisfactionNew, cart.predict, deparse.level = 2)

# Overall accuracy
mean(cart.predict == sac_testset$JobSatisfactionNew)
## 0.9137261

# Variable importance
sac.cart2$variable.importance
## MonthlyIncomeNew has the highest importance, followed by MonthlyRateNew


# Logistic model: Attrition ----
# Libraries used
library(nnet)
summary(att_trainset)
# Check baseline for AttritionNew
levels(att_trainset$AttritionNew) # baseline is 1

# Att.log1 with all X variables
att.log1 <- multinom(AttritionNew ~ ., data = att_trainset)
summary(att.log1)

# Odds ratio
OR.att_log1 <- exp(coef(att.log1))
OR.att_log1
OR.CI.att_log1 <- exp(confint(att.log1))
OR.CI.att_log1 
# all are significant. Years/JobInvolvementNew/JobSatisfactionNew being higher means AttritionNew is lower (factor 0 = "Current employee")

# Z-test
z <- summary(att.log1)$coefficients/summary(att.log1)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue
## All variables are statistically significant (p value < 0.05)

# Test att.log1 model
log.predict_prob <- predict(att.log1, newdata = att_testset, type = 'prob')
log.predict <- predict(att.log1, newdata = att_testset, type = 'class')
table(att_testset$AttritionNew, log.predict, deparse.level = 2) #imbalanced predictions
## Have zero predictions of voluntary resignation
## Current employee be the "positive" case
## Bad model, does not predict minority class...

## True positive rate = TP/(TP + FN) = 0

mean(log.predict == att_testset$AttritionNew) # Overall Accuracy = 0.8434

## creating att.log2 on a SMOTE balanced dataset
library(DMwR)
set.seed(2004)
att.log2 <- SMOTE(AttritionNew ~ ., data = att_trainset, 200, 10, 130, "multinom") 
# oversample minority class by 2x of original cases and undersample majority class 
# such that is is 2.5x of the minority cases
# 3:2.5

summary(att.log2)

# Odds ratio
OR.att_log2 <- exp(coef(att.log2))
OR.att_log2
OR.CI.att_log2 <- exp(confint(att.log2))
OR.CI.att_log2 # all variables are significant

# Z-test
z <- summary(att.log2)$coefficients/summary(att.log2)$standard.errors
pvalue <- (1 - pnorm(abs(z), 0, 1))*2  # 2-tailed test p-values
pvalue
## All variables are statistically significant (p value < 0.05)

# Test att.log2 model
log.predict_prob <- predict(att.log2, newdata = att_testset, type = 'prob')
log.predict <- predict(att.log2, newdata = att_testset, type = 'class')
table(att_testset$AttritionNew, log.predict, deparse.level = 2)
## Current employee be the "positive" case
## total pos in trainset = 5856, total neg in trainset = 1087

## True positive rate = TP/(TP + FN) = 652/(652+2995) = 0.178

# Overall Accuracy
mean(log.predict == att_testset$AttritionNew) # accuracy = 0.56, better!

# CART Model: Attrition ----
library(DMwR)
library(rpart)
library(rpart.plot)
set.seed(2004)
att.cart3 <- SMOTE(AttritionNew ~ ., data = att_trainset, 200, 10, 130,
                   "rpart", method = 'class',
                   control = rpart.control(minsplit = 12, cp = 0))

print(att.cart3)

# Shows pruning sequence
printcp(att.cart3)
plotcp(att.cart3, main = "Subtrees in att_trainset") ## too many trees --> use while loop to find cp.opt

# While loop to extract optimal cp to prune
## Compute CVerror.cap = min CVerror + 1SE in maximal tree sac.cart1
att_CVerror.cap <- att.cart3$cptable[which.min(att.cart3$cptable[,"xerror"]), "xerror"] + att.cart3$cptable[which.min(att.cart3$cptable[,"xerror"]), "xstd"]
## Find the optimal CP
i <- 1; j<- 4
while (att.cart3$cptable[i,j] > att_CVerror.cap) {
  i <- i + 1
} ## i=29 --> optimal tree

## Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
att_cp.opt = ifelse(i > 1, sqrt(att.cart3$cptable[i,1] * att.cart3$cptable[i-1,1]), 1)

att_cp.opt = sqrt(att.cart3$cptable[i,1] * att.cart3$cptable[i-1,1]) # cp.opt = 0.000428

# Prune att_cp.opt
att.cart4 <- prune(att.cart3, cp = att_cp.opt)
#printcp(att.cart4)
#rpart.plot(att.cart4, nn= T, main = "Pruned Tree with cp = 0.000194792") ## left 850 split & 851 terminal nodes

# Test att.cart4 model
att.predict <- predict(att.cart4, newdata = att_testset, type = "class")
table(att_testset$AttritionNew, att.predict, deparse.level = 2)

# Overall accuracy
mean(att.predict == att_testset$AttritionNew)
## with balanced data -> 0.732 (good enough!)

# Variable importance
att.cart4$variable.importance