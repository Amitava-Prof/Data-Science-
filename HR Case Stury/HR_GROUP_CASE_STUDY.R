### Business Understanding:

################################################################

## AIM:
# The aim of analysis is to identify the root cause of employee attrition and 
# to understand what factors needs to be focussed on, in order to curb attrition. 
# In other words, they want to know what changes they should make to their workplace, 
# in order to get most of their employees to stay.


################################################################

### Data Understanding
# Load the required packages
library(stringr)
library(dplyr)
library(MASS)
library(car)
library(e1071)
library(ggplot2)
library(lattice)
library(caret)
library(cowplot)
library(caTools)


# Loading 5 files
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time_data <- read.csv("in_time.csv", stringsAsFactors = F)
out_time_data <- read.csv("out_time.csv", stringsAsFactors = F)

str(general_data)         # 4410 obs of 24 variables including the target variable
str(employee_survey_data) # 4410 obs of 4 variables
str(manager_survey_data)  # 4410 obs of 3 variables
str(in_time_data)         # 4410 obs of 262 variables
str(out_time_data)        # 4410 obs of 262 variables

# Collate the data together in one single file 
length(unique(tolower(general_data$EmployeeID)))            # 4410, confirming EmployeeID is key
length(unique(tolower(employee_survey_data$EmployeeID)))    # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID)))     # 4410, confirming EmployeeID is key
length(unique(tolower(in_time_data$X)))                     # 4410, confirming X is key
length(unique(tolower(out_time_data$X)))                    # 4410, confirming X is key

setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID)  # Identical EmployeeID across these datasets

setdiff(general_data$EmployeeID,in_time_data$X) # Identical EmployeeID with X across these datasets
setdiff(in_time_data$X,out_time_data$X)         # Identical X across these datasets

employee <- merge(general_data,employee_survey_data, by="EmployeeID", all = F)
employee <- merge(employee,manager_survey_data, by="EmployeeID", all = F)

# CReating new columns - WorkingHoursAdherence and NoOfLeaves per employee from in_data and out_data
in_time <- in_time_data[,-1]
in_time <- Filter(function(x)!all(is.na(x)), in_time) # Remove all NA columns
out_time <- out_time_data[,-1]
out_time <- Filter(function(x)!all(is.na(x)), out_time) # Remove all NA columns
#converting string to datetime column  
sapply(in_time,function(x) as.POSIXct(x,format='%Y-%m-%d %H:%M:%S',tz="UTC",origin = "1970-01-01"))
sapply(in_time,function(x) as.POSIXct(x,format='%Y-%m-%d %H:%M:%S',tz="UTC",origin = "1970-01-01"))
# Get the time difference of in_time and out_time to find the time spent by an employee in office
timediff = mapply(function(a,b){difftime(a,b,units="hours")},a=out_time,b=in_time)
avg = rowMeans(timediff, na.rm = TRUE)
NoOfLeave = apply(timediff, 1, function(x) sum(is.na(x)))
# Create calculated field StandardHourCompliance - 1 if the employee's average working hour is >= 8 and 0 if it is less than 8 hours
StandardHourCompliance = sapply(avg,function(x) if(x>8) 1 else 0)
employee$StandardHourCompliance = StandardHourCompliance
# Create calculated field NoOnLeave per employee
employee$NoOfLeave = NoOfLeave
################################################################

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(employee) #4410 obs. of 31 variables;

# EmployeeID is the unique ID
# Age is interger, no conversion needed , no NULL value
sum(is.na(employee$Age))
summary(employee$Age)
# Attrition is the outcome - no NULL value, 3699 - No and 711 - Yes
sum(is.na(employee$Attrition))
table(employee$Attrition)
# BusinessTravel - character variable - 3 values - Non-Travel - 450, Travel-Frequently-831, Travel_Rarely - 3129 
sum(is.na(employee$BusinessTravel))
table(employee$BusinessTravel)
# Department - Categorical Variable - 3 values - Human Resources - 189, Research & Development - 2883, Sales - 1338 
sum(is.na(employee$Department))
table(employee$Department)
# DistanceFromHome - Continuous variable 
sum(is.na(employee$DistanceFromHome))
summary(employee$DistanceFromHome)
# Education - Continuous variable 
sum(is.na(employee$Education))
summary(employee$Education)
# EducationField - Categorical Variable - 6 values - Human Resources -81, Life Science - 1818, Marketing - 477, Medical - 1392, Other - 246, Technical Degree - 396
sum(is.na(employee$EducationField))
table(employee$EducationField)
# EmployeeCount - interger variable - value 1 for all rows - Column can be deleted 
sum(is.na(employee$EmployeeCount))
table(employee$EmployeeCount)
# Gender - categorical variable - 2 values - Female - 1764, Male - 2646
sum(is.na(employee$Gender))
table(employee$Gender)
# JobLevel - Continuous variable - value range 1 to 5
sum(is.na(employee$JobLevel))
summary(employee$JobLevel)
# JobRole - Categorical Variable - 9 values
sum(is.na(employee$JobRole))
table(employee$JobRole)
# MaritalStatus - Categorical Variable - 3 values - Divorced - 981, Married - 2019, Single - 1410
sum(is.na(employee$MaritalStatus))
table(employee$MaritalStatus)
# MonthlyIncome - Continuous Variable - Range 10090 to 199990 - outlier exists
sum(is.na(employee$MonthlyIncome))
summary(employee$MonthlyIncome)
# NumCompaniesWorked - Continuous variable - 19 NULL value Range 0 to 9 , outlier
sum(is.na(employee$NumCompaniesWorked))
summary(employee$NumCompaniesWorked)
# Over18 - Character - All Y values - Column can be deleted
sum(is.na(employee$Over18))
table(employee$Over18)
# PercentSalaryHike - Continuous Variable - Range 11 to 25
sum(is.na(employee$PercentSalaryHike))
summary(employee$PercentSalaryHike)
# StandardHours - Integer - All 8 values - Column can be deleted
sum(is.na(employee$StandardHours))
table(employee$StandardHours)
# StockOptionLevel - Integer - Continuous variable - Range 0 to 3
sum(is.na(employee$StockOptionLevel))
summary(employee$StockOptionLevel)
# TotalWorkingYears - Integer - Continuous Variable - 9 NULL values - Range 0 to 40 
sum(is.na(employee$TotalWorkingYears))
summary(employee$TotalWorkingYears)
# TrainingTimesLastYear - Integer - Continuous variable - Range 0 to 6
sum(is.na(employee$TrainingTimesLastYear)) 
summary(employee$TrainingTimesLastYear)
# YearsAtCompany - Integer - Continuous Variable - Range 0 to 40
sum(is.na(employee$YearsAtCompany)) 
summary(employee$YearsAtCompany)
# YearsSinceLastPromotion - Integer - Continous Variable - Range 0 to 15
sum(is.na(employee$YearsSinceLastPromotion)) 
summary(employee$YearsSinceLastPromotion)
# YearsWithCurrManager - Integer - Continuous Variable - Range 0 to 17
sum(is.na(employee$YearsWithCurrManager)) 
summary(employee$YearsWithCurrManager)
# EnvironmentSatisfaction - Integer - Continuous Variable - 25 NULL values - Range 1 to 4
sum(is.na(employee$EnvironmentSatisfaction)) 
summary(employee$EnvironmentSatisfaction)
# JobSatisfaction - Integer - Continuous Variable - 20 NULL values - Range 1 to 4
sum(is.na(employee$JobSatisfaction)) 
summary(employee$JobSatisfaction)
# WorkLifeBalance - Integer - Continuous Variable - 38 NULL value - Range 1 to 4
sum(is.na(employee$WorkLifeBalance)) 
summary(employee$WorkLifeBalance)
# JobInvolvement - Integer - Continuous Variable - Range 1 to 4
sum(is.na(employee$JobInvolvement)) 
summary(employee$JobInvolvement)
# PerformanceRating - Integer - Continuous Variable - Range 3 to 4 
sum(is.na(employee$PerformanceRating)) 
summary(employee$PerformanceRating)

# Barcharts for categorical features with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(employee, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(), 
          ggplot(employee, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

### Data Preparation

# Delete 3 variables - EmployeeCount, Over18, StandardHours
drops <- c("EmployeeCount","Over18", "StandardHours")
employee <- employee[ , !(names(employee) %in% drops)]

#Number of rows having NA values
row.has.na <- apply(employee, 1, function(x){any(is.na(x))})
sum(row.has.na)
#total 110 rows has NULL values 2.5% data having NULL values removing them all
employee <- employee[!row.has.na,]
#Converting "BusinessTravel" into dummies . 
dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = employee))
dummy_1 <- dummy_1[,-1]
employee <- cbind(employee, dummy_1)

#Converting "Department" into dummies . 
dummy_1 <- data.frame(model.matrix( ~Department, data = employee))
dummy_1 <- dummy_1[,-1]
employee <- cbind(employee, dummy_1)

#Converting "EducationField" into dummies . 
dummy_1 <- data.frame(model.matrix( ~EducationField, data = employee))
dummy_1 <- dummy_1[,-1]
employee <- cbind(employee, dummy_1)

#Converting "MaritalStatus" into dummies . 
dummy_1 <- data.frame(model.matrix( ~MaritalStatus, data = employee))
dummy_1 <- dummy_1[,-1]
employee <- cbind(employee, dummy_1)

#Converting categorical variable with 2 different values
# One simple way to convert Gender variable to numeric is to replace the levels- Male's and Female's with 0 and 1 is:
employee$Gender <- as.factor(employee$Gender)
levels(employee$Gender)<-c(1,0)
employee$Gender<- as.numeric(levels(employee$Gender))[employee$Gender]

#Converting outcome variable Attrition with 2 different values from categorical to numeric as model needs that
# One simple way to convert Attrition variable to numeric is to replace the levels- Yes's and No's with 0 and 1 is:
employee$Attrition <- as.factor(employee$Attrition)
levels(employee$Attrition)<-c(1,0)
employee$Attrition<- as.numeric(levels(employee$Attrition))[employee$Attrition]


########################################################################
# splitting the data between train and test
set.seed(100)
# Removing the EmployeeId column as this will ot add any value to the modeling
employee_final <- employee[,-1]

indices = sample.split(employee_final$Attrition, SplitRatio = 0.7)

train = employee_final[indices,]

test = employee_final[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2098.9......nullDev 2661.4...resDev 2016.9

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

#Excluding .........JobRole & JobLevel
model_3 <- glm(formula = Attrition ~ Age + BusinessTravel + EducationField + 
          MaritalStatus + NumCompaniesWorked + PerformanceRating + 
          TotalWorkingYears + TrainingTimesLastYear + 
          YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
          JobSatisfaction + WorkLifeBalance + StandardHourCompliance, 
          family = "binomial", data = train)

summary(model_3)

#Excluding .........PerformanceRating
model_4 <- glm(formula = Attrition ~ Age + BusinessTravel + EducationField + 
                 MaritalStatus + NumCompaniesWorked +  
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + StandardHourCompliance, 
               family = "binomial", data = train)

summary(model_4)


#Excluding ......... TrainingTimesLastYear 
model_5 <- glm(formula = Attrition ~ Age + BusinessTravel + EducationField + 
                 MaritalStatus + NumCompaniesWorked +  
                 TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + StandardHourCompliance, 
               family = "binomial", data = train)

summary(model_5)


#Excluding ......... MaritalStatus 
model_6 <- glm(formula = Attrition ~ Age + BusinessTravel + EducationField + 
                 NumCompaniesWorked +  
                 TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + StandardHourCompliance, 
               family = "binomial", data = train)

summary(model_6)

#Excluding ......... BusinessTravel 
model_7 <- glm(formula = Attrition ~ Age + EducationField + 
                 NumCompaniesWorked +  
                 TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + StandardHourCompliance, 
               family = "binomial", data = train)

summary(model_7)



#including ......... TrainingTimesLastYear 
model_8 <- glm(formula = Attrition ~ Age + EducationField + 
                 NumCompaniesWorked + 
                 TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + StandardHourCompliance, 
               family = "binomial", data = train)

summary(model_8)

#including ......... BusinessTravel

model_9 <- glm(formula = Attrition ~ Age + EducationField + 
                 NumCompaniesWorked +   BusinessTravel + TrainingTimesLastYear+
               TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + StandardHourCompliance, 
               family = "binomial", data = train)

summary(model_9)

#excluding ......... TrainingTimesLastYear

model_10 <- glm(formula = Attrition ~ Age + EducationField + 
                 NumCompaniesWorked +   BusinessTravel +
                 TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + StandardHourCompliance, 
               family = "binomial", data = train)

summary(model_10)

vif(model_10)

# After repeating the iteration derive the final model
final_model<- model_10

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test)


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)
#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition, positive = "Yes")

test_conf
#######################################################################

#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 


perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive="Yes")
  conf
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Let's choose a cutoff value of 0.8 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.8, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

View(test)

### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(as.numeric(test_cutoff_attrition), as.numeric(test_actual_attrition))
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

####################################################################
# Lift & Gain Chart 

lift <- function(labels , predicted_prob,groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(as.factor(test_actual_attrition), test_pred, groups = 10)



