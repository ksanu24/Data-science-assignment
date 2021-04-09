#classify the Size_Categorie using SVM

#month	month of the year: 'jan' to 'dec'
#day	day of the week: 'mon' to 'sun'
#FFMC	FFMC index from the FWI system: 18.7 to 96.20
#DMC	DMC index from the FWI system: 1.1 to 291.3
#DC	DC index from the FWI system: 7.9 to 860.6
#ISI	ISI index from the FWI system: 0.0 to 56.10
#temp	temperature in Celsius degrees: 2.2 to 33.30
#RH	relative humidity in %: 15.0 to 100
#wind	wind speed in km/h: 0.40 to 9.40
#rain	outside rain in mm/m2 : 0.0 to 6.4
#Size_Categorie 	the burned area of the forest ( Small , Large)


library(caret)
install.packages("kernlab")
library(kernlab)
install.packages("ggvis")
library(ggvis)
library(psych)

forest_svm <- read.csv("C:/datasciences/asssignments/Support Vector Machine/forestfires.csv")
View(forest_svm)

#feature selection
forest_svm <- forest_svm[,-c(1,2,12:30)]
View(forest_svm)

#data visualizaton
forest_svm %>% ggvis(~FFMC, ~DMC,fill = ~ forest_svm$size_category) %>% layer_points()
forest_svm %>% ggvis(~FFMC, ~DC,fill = ~ forest_svm$size_category) %>% layer_points()
forest_svm %>% ggvis(~temp, ~RH,fill = ~forest_svm$size_category) %>% layer_points()
forest_svm %>% ggvis(~RH, ~rain,fill = ~forest_svm$size_category) %>% layer_points()
forest_svm %>% ggvis(~area, ~RH,fill = ~forest_svm$size_category) %>% layer_points()
forest_svm %>% ggvis(~area, ~temp,fill = ~forest_svm$size_category) %>% layer_points()
forest_svm %>% ggvis(~area, ~rain,fill = ~forest_svm$size_category) %>% layer_points()
forest_svm %>% ggvis(~area, ~wind,fill = ~forest_svm$size_category) %>% layer_points()
# The above analysis indicates that the burnt area increases in in high temp and medium wind speed
# The burnt area increases at low/ medium level humidity and no rain conditions
# Understanding the pattern of area affected in forest fire


#data preprocessing 
colnames(forest_svm)

anyNA(forest_svm)  #no NA's

#feature convert into factor
forest_svm$size_category <- factor(forest_svm$size_category)

summary(forest_svm)

#data normalization
# Therefore, we can normalise the data
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) 
}
# Applying normalisation to all the important variables
forest_svm$temp <- normalise(forest_svm$temp)
forest_svm$rain <- normalise(forest_svm$rain)
forest_svm$RH <- normalise(forest_svm$RH)
forest_svm$wind <- normalise(forest_svm$wind)

#splitting data set into training and test set
library(caTools)
set.seed(123)
split = sample.split(forest_svm$size_category,SplitRatio = 0.7)
F_train = subset(forest_svm,split == TRUE)
F_test = subset(forest_svm,split == FALSE)

dim(F_train)
dim(F_test)

##this will control all the computational overheads so that we can use the train() function provided by the caret package. 
##The training method will train our data on diffrent algorithms.
trctrl <- trainControl(method = "repeatedcv" ,number = 10, repeats = 3)

#fitting classifier
library(e1071)
library(ggplot2)
svm_class = svm(formula = size_category ~temp + RH + wind + rain,
                  data = F_train,
                  trcontrol = trctrl,
                  type = "C-classification",
                  preProcess = c("center","scale"),
                  method = "svmLinear",
                  tuneLength = 10)
svm_class
#predicting the test set results
y_pred = predict(svm_class, newdata = F_test[-10])

#confusion matrix
confusionMatrix(table(F_test$size_category ,y_pred)) #accuracy found 75%

##here i do some customization for selecting C value(Cost) in Linear classifier. 
##This can be done by inputting values in grid search. 
grid <- expand.grid(C = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 5))
set.seed(123)
svm_linear_grid <- train(size_category ~ temp + RH + wind + rain,data = F_train,
                        method = "svmLinear",
                        trcontrol = trctrl,
                        preProcess = c("center","scale"),
                        tuneGrid = grid,
                        tuneLength = 10)
svm_linear_grid
plot(svm_linear_grid)
#predicting the test set results
grid_pred = predict(svm_linear_grid, newdata = F_test[-10])
#confusion matrix
confusionMatrix(table(F_test$size_category,grid_pred))
#accuracy found 73% in grid linear model which is decrease than previous model 

#model using "polydot" kernel
model_poly = train(size_category ~ temp + RH + wind + rain, data = F_train,
                  trcontrol = trctrl,
                  type = "C-svc",
                  kernel = "polydot",
                  tuneLength = 10)

model_poly
#prediction 
poly_pred = predict(model_poly, newdata = F_test[-10])

#confusion matrix
confusionMatrix(table(F_test$size_category,poly_pred))#accuracy is 72.26%

plot(model_poly)

#kernel svm radial basis which is "rbfdot"
model_rad = train(size_category ~ temp + RH + wind + rain, data = F_train,
                  trcontrol = trctrl,
                  type = "C-svc",
                  kernel = "rbfdot",
                  tuneLength = 10)
model_rad
#prediction 
rad_pred = predict(model_rad, newdata = F_test[-10])

#confusion matrix
confusionMatrix(table(F_test[,10],rad_pred)) #accuracy is 72.26% same here

plot(model_rad)

#kernel svm using "tanhdot" which is the complicated sounding hyperbolic tangentsigmoid. 
model_tanh = train(size_category ~ temp + RH + wind + rain, data = F_train,
                   trcontrol = trctrl,
                   type = "C-svc",
                   kernel = "tanhdot",
                  tuneLength = 10)
model_tanh
#prediction 
tanh_pred = predict(model_tanh, newdata = F_test[-10])

#confusion matrix
confusionMatrix(table(F_test$size_category,tanh_pred)) #accuracy is 73% 

plot(model_tanh)

## Conclusion ##
#Accuracy compare here in 5 types of models with simple svm linear and kernel linear
#so better accuracy found in svm linear model which is 75%
# and then another better accuracy found in tanh sigmoid kernel model 
#which is 73%
