#Insert Data.
Data=read.table("DataMLRegre.data",header=T,na.strings ="?")

#Method 1: Multiple Linear Regression
#Standarize the data and save them into a matrix.
standData = scale(Data, center=TRUE, scale=TRUE)
#Convert the matrix to dataframe and attach it.
DataFrame = data.frame(standData)
attach(DataFrame)
#Split the data for training and testing.
set.seed(1)
train=sample(506,253)
#Create mul/ple regression model with all the variables.
lm.fit=lm(effort~.,data=DataFrame,subset=train)
#Information about the model.
summary(lm.fit)
#Check for collinearity.
alias(lm.fit)
#Create diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)
#Calculate Mean Squared Error.
mean((effort-predict(lm.fit,DataFrame))[-train]^2)
#Create a dataset containing only the test data.
Test.data.matrix = standData[-train,]
Test.data = data.frame(Test.data.matrix)
#Import boot library
library(boot)
#Using bootstrap, resample with replacement the test data and calculate the confidence intervals for the MSE.
boot.fn=function (data ,index){+ mean((data$effort-predict(lm.fit,data[index,]))^2)}
btstrp = boot(Test.data ,boot.fn ,1000)
c(btstrp$t0 - 2 * 0.09330121, btstrp$t0 + 2 * 0.09330121)

#Method 2: Subset Selection
library(leaps)
#Run best subset selection.
regfit.full=regsubsets(effort~.,DataFrame)
summary(regfit.full)
#Plotting RSS, adjusted R^2, Cp, and BIC for all the models at once in order to decide which model to select.
reg.summary = summary(regfit.full)
#RSS
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
#Adjusted R^2
plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(5,reg.summary$adjr2[5], col="red",cex=2,pch =20)
#Cp
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp",type="l")
which.min(reg.summary$cp)
points(5,reg.summary$cp [5], col ="red",cex=2,pch =20)
#BIC
which.min(reg.summary$bic)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
#Plotting the selected variables for the best model with a given number of predictors, ranked according to the above statistics.
plot(regfit.full ,scale="r2")
plot(regfit.full ,scale="adjr2")
plot(regfit.full ,scale="Cp")
plot(regfit.full ,scale="bic")
#Performing forward and backward selection in order to decide which 5-variable model to select.
regfit.fwd=regsubsets(effort~.,DataFrame,method='forward')
regfit.bwd=regsubsets(effort~.,DataFrame,method ="backward")
coef(regfit.full ,5)
coef(regfit.fwd ,5)
coef(regfit.bwd ,5)
#Perform multiple linear regression with the variables previously selected and calculate MSE.
lm.fit = lm(effort~AdjustedFunctionPoints+Inputcount+Enquirycount+Filecount+Interfacecount,data=DataFrame, subset=train)
mean((effort-predict(lm.fit,DataFrame))[-train]^2)
#Using bootstrap, calculate the confidence intervals for the MSE.
btstrp = boot(Test.data ,boot.fn ,1000)
c(btstrp$t0 - 2 * 0.102879, btstrp$t0 + 2 * 0.102879)

#Method 3: Non Linear Regression
#Using poly() check if including higher order polynomial terms of AdjustedFunctionPoints variable up to 5, improves the model.
lm.fit2=lm(effort~poly(AdjustedFunctionPoints,5)+Inputcount+Enquirycount+Filecount+Interfacecount,data=DataFrame,subset=train)
#Keep the second order polynomial of AdjustedFunctionPoints.
lm.fit2=lm(effort~poly(AdjustedFunctionPoints,2)+Inputcount+Enquirycount+Filecount+Interfacecount,data=DataFrame,subset=train)
#Calculate test MSE.
mean((effort-predict(lm.fit2,DataFrame))[-train]^2)
#Using anova compare the models.
anova(lm.fit ,lm.fit2)
#Using bootstrap, calculate the confidence intervals for the MSE.
boot.fn=function (data ,index){+ mean((data$effort-predict(lm.fit2,data[index,]))^2)}
btstrp = boot(Test.data ,boot.fn ,1000)
c(btstrp$t0 - 2 * 0.03660796, btstrp$t0 + 2 * 0.03660796)