library(readxl)
library(ggplot2)
library(ggcorrplot)
library(ellipse)
library(RColorBrewer)
library(nFactors)
library(psych)
library(lattice)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(data.table)
library(ROCR)
library(ineq)
library(StatMeasures)
library(htmlwidgets)
library(DataExplorer)
library(corrplot)
library(partykit)
library(dplyr)
library(purrr)
library(InformationValue)
library(car)
library(ROCR)
library(MASS)
library(e1071)
library(class)
library(caret)




setwd("C:/Users/Samrat/Documents/R/Directories/")
getwd()
data = read_excel("Cellphone.xlsx")
head(data)

names(data)
summary(data)
str(data)
attach(data)
data = as.data.frame(data)



hist(DataUsage,density = 20, col = "black")
hist(AccountWeeks,density = 20, col = "blue")
hist(DayMins, col = "red")
hist(DayCalls, col = "lightblue")
hist(MonthlyCharge, density = 20, col = "orange")
hist(RoamMins, density = 20, col = "green")


plot(AccountWeeks,DayMins)
lm(DayMins~AccountWeeks)
abline(1.789e+02,8.502e-03)
summary(lm(DayMins~AccountWeeks))

plot(AccountWeeks,Churn)
lm(Churn~AccountWeeks)
abline(0.1301351,0.0001462)
summary(lm(Churn~AccountWeeks))

plot(DayMins,Churn)
lm(Churn~DayMins)
abline(-0.093478,0.001326)
summary(lm(Churn~DayMins))

plot(OverageFee,Churn)
lm(Churn~OverageFee)
abline(0.01539,0.01289)
summary(lm(Churn~OverageFee))

plot(MonthlyCharge,Churn)
lm(Churn~MonthlyCharge)
abline(0.05765,0.00155)
summary(lm(Churn~MonthlyCharge))




lm(Churn~., data = data)
summary(lm(Churn~., data = data))

summary(lm(Churn~ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data))


sum(is.na(data))

boxplot(data[,c(-1,-3)], col = "lightblue")


corr.matrix = round(cor(data),3) 
corr.matrix
plot_correlation(data)

ggcorrplot(corr.matrix, type = "lower", ggtheme = ggplot2::theme_gray,
           show.legend = TRUE, show.diag = TRUE, colors = c("cyan","white","sky blue"),
           lab = TRUE)

my_colors = brewer.pal(7, "Blues")
my_colors = colorRampPalette(my_colors)(100)
plotcorr(corr.matrix , col=my_colors[corr.matrix*50+50] , mar=c(1,1,1,1), )

plot(DataPlan,DataUsage)
lm(DataUsage~DataPlan)
abline(lm(DataUsage~DataPlan))
summary(lm(DataUsage~DataPlan))


lm.Churn = lm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data)
predict(lm.Churn)[1]
residuals(lm.Churn)[1]
Churn[1]

vif(lm.Churn)

plot(Churn,predict(lm.Churn))
cor(Churn,predict(lm.Churn))^2

hist(residuals(lm.Churn))
summary(lm.Churn)



set.seed(1)
index = sample(3333,833)

test.data = data[index,]
dim(test.data)
train.data = data[-index,]
dim(train.data)


lm.train.Churn = lm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = train.data)
summary(lm.train.Churn)



prediction = predict(lm.train.Churn, newdata = test.data)
plot(test.data$Churn,prediction)
cor(test.data$Churn,prediction)^2

#Using this model we can explain about 18 % of the variation in Churn for new observations of the predictor variables 
#which are in this case ContractRenewal, CustServCalls, RoamMins, DayMins, DataUsage and OverageFee.

vif(lm(Churn~., data = data))
vif(lm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data))




glm(Churn~., data = data, family = "binomial")
summary(glm(Churn~., data = data, family = "binomial"))


glm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data, family = "binomial")
summary(glm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data, family = "binomial"))

vif(glm(Churn~., data = data, family = "binomial"))

vif(glm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data, family = "binomial"))

# By removing DataPlan and MOnthlyCharge we can get rid of multicollinearity in the data as evidenced by the Variance Inflation Factor function.

logistic.Churn = glm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data, family = "binomial")

exp(0.0980337)
exp(0.0127369)

#Therefore, every extra avergae roaming minute used by the customer increases the odds of Churn by 10.3% and every unit increase in the average number
#number of Daytime minutes used by the customer the odds of Churn increases by 1.2%.

#Contract renewal and data usage have negative slopes which indictes that they have a negative realtion to Churn

exp(-1.9900101)-1
exp(-0.3047374)-1

#Hence, for every customer that renews their contract, the probability of Churn decreases by 86.3%
#And for every extra gigabyte of data used by the customer, the odds of Churn decreases by 26.2%


boxplot(Churn,logistic.Churn$fitted.values)

#The graph indictaes that all customers having a Churn value above the threshold of 0.2 in the prediction model, have a higher probability of cancelling their service.


Churn.predicted = ifelse(logistic.Churn$fitted.values<0.2,"0","1")
table(Churn.predicted)
table(Churn)

#Sensitivity
2565/2850

#Specificity
483/768


Log.Reg = table(Churn, Churn.predicted)

confusionMatrix(Log.Reg)

perf = performance(prediction(logistic.Churn$fitted.values,Churn),"tpr","fpr")
plot(perf)
auc(Churn,logistic.Churn$fitted.values)

#Therefore, we get a model with an area under the ROC curve of 79.7%. This indicates that there is an 79.7% chance that the model will 
#correctly identify a customer who will retain or cancel their services with the telecom company.


lda.Churn = lda(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data)
lda.Churn

#Linear Discriminant Analysis tells us that about 85.5% retain their services while 14.49% cancel their services


NB.Churn = naiveBayes(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = data)
pred.NB = predict(NB.Churn, type = "raw", newdata = data)
boxplot(Churn,pred.NB[,2])


# Hence Naive-Bayes is applicable here and this model gives us a lower threshold value of about 0.04.



knn(train.data[,c(-4,-9)],test.data[,c(-4,-9)],train.data$Churn,k=5)
KNN.Churn = knn(train.data[,c(-4,-9)],test.data[,c(-4,-9)],train.data$Churn,k=1)
table(test.data$Churn,KNN.Churn)

glm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = train.data, family = "binomial")

predict(glm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = train.data, family = "binomial"), newdata = test.data, type = "response")
prediction.KNN = predict(glm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = train.data, family = "binomial"), newdata = test.data, type = "response")
boxplot(predict(glm(Churn~DayCalls+AccountWeeks+ContractRenewal+CustServCalls+RoamMins+DayMins+DataUsage+OverageFee, data = train.data, family = "binomial"), newdata = test.data, type = "response"))


auc(test.data$Churn,prediction.KNN)


table(Churn,)
#Here we get an area under the curve value of 80.14% which is slightly better than the Logistic Regression model.   