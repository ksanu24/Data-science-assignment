#Prepare a classification model using SVM for salary data 

#Data Description:

# age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	
# race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual


# Loading the required packages

install.packages("kernlab")
install.packages("ggplot2")
install.packages("caret")
install.packages("psych")
library(kernlab)
library(ggplot2)
library(caret)
library(psych)

# Loading the Salary Dataset
SD_Train <-read.csv("C:/datasciences/asssignments/Support Vector Machine/SalaryData_Train.csv")
SD_Test <-read.csv("C:/datasciences/asssignments/Support Vector Machine/SalaryData_Test.csv")
View(SD_Train)
str(SD_Train)
summary(SD_Train)

# plotting the salary against all fields

plot(factor(SD_Train$Salary),factor(SD_Train$age))
plot(factor(SD_Train$Salary),factor(SD_Train$workclass))
plot(factor(SD_Train$Salary),factor(SD_Train$education))
plot(factor(SD_Train$Salary),factor(SD_Train$educationno))
plot(factor(SD_Train$Salary),factor(SD_Train$maritalstatus))
plot(factor(SD_Train$Salary),factor(SD_Train$occupation))
plot(factor(SD_Train$Salary),factor(SD_Train$relationship))

plot(factor(SD_Train$Salary),factor(SD_Train$race))
plot(factor(SD_Train$Salary),factor(SD_Train$sex))
plot(factor(SD_Train$Salary),factor(SD_Train$capitalgain))
plot(factor(SD_Train$Salary),factor(SD_Train$capitalloss))
plot(factor(SD_Train$Salary),factor(SD_Train$hoursperweek))
plot(factor(SD_Train$Salary),factor(SD_Train$native))



# converting the size category to factor

SD_Train$Salary <- factor(SD_Train$Salary)
table(SD_Train$Salary) # Majority class variable value <=50K


#Training a model on the data ----
# a simple linear SVM
colnames(SD_Train)
model1<- ksvm(Salary ~ .,
              data= SD_Train, kernel = "vanilladot")
model1 # Training error - 0.15, Support Vectors - 10593

## Evaluating model performance ----
# predictions on testing dataset
Area_pred <- predict(model1, SD_Test)
confusionMatrix(table(Area_pred,SD_Test$Salary))
# Accuracy is high - 0.84 % however, 
# No Information Rate : 0.75 i.e 75% chance of sal <=50K without applying any model
# Sensitivity : 0.93 which means it is giving 93% correct prediction for salary >50K
# Specificity : 0.58 which means it is giving 58% correct prediction for Salary <=50K       



#Improving model performance ----
model_rfdot <- ksvm(Salary ~ .,
                    data= SD_Train,kernel = "rbfdot")
model_rfdot # Training error - 0.13 , Support Vectors - 199

# Model performance evaluation , prediction on test data

pred_rfdot<-predict(model_rfdot,SD_Test)
confusionMatrix(table(pred_rfdot, SD_Test$Salary))
# Accuracy is 0.85         
# No Information Rate : 0.75 remains same as above
# Sensitivity : 0.93 
# Specificity : 0.59


# Improving model performance ----
#   By using the non-linear model "besseldot"
model_besseldot <- ksvm(Salary ~ ., 
                        data= SD_Train,kernel = "besseldot")
model_besseldot # Training error - 0.22 , Number of Support Vectors - 8255
pred_bessel <- predict(model_besseldot,SD_Test)
confusionMatrix(table(pred_bessel,SD_Test$Salary))
# Accuracy is 0.77 
# No Information Rate : 0.75 remains same as above
# Sensitivity : 0.85
# Specificity : 0.52  



## Further improving model performance by using plydot:
model_poly <- ksvm(Salary ~ .,
                   data= SD_Train,kernel = "polydot")
model_poly # Training error - 0.15 Number of support vectors 10591
pred_poly<-predict(model_poly,SD_Test)
confusionMatrix(table(pred_poly,SD_Test$Salary))

# Accuracy is 0.84    
# No Information Rate : 0.75 remains same as above
# Sensitivity : 0.93
# Specificity : 0.58 


# Conclusion:
#  Besseldot model is good as accuracy is high 0.85


