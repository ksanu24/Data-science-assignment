W <- read.csv(file.choose())
View(W) # view the data set

attach(W)
#Measure of central tendency - 1st business moments
#Mean
mean(Weight.gained..grams.)
mean(Calories.Consumed)
#Median
median(Weight.gained..grams.)
median(Calories.Consumed)
#Mode
getmode <- function(x){
  uniquv <- unique(x)
  uniquv[which.max(tabulate(match(x,uniquv)))]
}
getmode(Weight.gained..grams.)
getmode(Calories.Consumed)

#Meaure of Dispersion - 2nd business moments
#variance
var(Weight.gained..grams.)
var(Calories.Consumed)
#Range
rangevalue=function(x){max(x)-min(x)}
range(Weight.gained..grams.)
range(Calories.Consumed)
#Standard deviation
sd(Weight.gained..grams.)
sd(Calories.Consumed)

#Measure of Skewness - 3rd business moments
install.packages("moments")
library(moments)
#measure of Skewness
skewness(Weight.gained..grams.) # skewness is postive
skewness(Calories.Consumed) # skewness is postive

#Measure of Kurtosis - 4th business moments
#measure of Kurtosis
kurtosis(Weight.gained..grams.)
kurtosis(Calories.Consumed)

#graphical representation
hist(Weight.gained..grams.) #Positive 
hist(Calories.Consumed) #Extending towards right side so, positive skewness

#outliears
x=boxplot(Weight.gained..grams.,Calories.Consumed) #No outliear found in both the variable
x$out # To check what are the outlier we have

str(W)


#install.packages(psych)
library(psych)
describe(W)
?describe

#qqplot
qqplot(Calories.Consumed,Weight.gained..grams.)
qqnorm(Calories.Consumed)
?qqnorm
qqline(Calories.Consumed)
qqnorm(Weight.gained..grams.)
qqline(Weight.gained..grams.)
#install.packages("lattice")
library(lattice)
#Scatter Plot
plot(Calories.Consumed,Weight.gained..grams.)

cor(Calories.Consumed,Weight.gained..grams.) #correlation is more than 0.85,So we can say that correlation is strong.

#Model
reg=lm(Calories.Consumed~Weight.gained..grams.,data = W) #Y~X

attach(W)
##Model
reg=lm(Calories.Consumed~Weight.gained..grams,data=W)
summary(reg)

confint(reg,level = 0.95)
predict(reg,interval = "predict")

