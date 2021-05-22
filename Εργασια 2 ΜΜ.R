#Insert Data.
Data=read.table("DataMLRegre.data",header=T,na.strings ="?")

#Convert the effort column into a binary variable containing the class variables we want to predict.
attach(Data)
effort.bin = rep("Low", length(effort))
#The variable's median is chosen as the limit of the 2 classes.
effort.bin[effort > median(effort)] = "High"
#Add the new column to the dataframe.
Data = data.frame(Data, effort.bin)
#Remove the effort column.
Data = subset(Data, select = -effort)

#Split the data into test and train.
#70% of the sample size goes into training and the rest into testing.
smp_size <- floor(0.70 * nrow(Data))
set.seed(1)
train_ind = sample(seq_len(nrow(Data)), size = smp_size)
train = Data[train_ind, ]
test = Data[-train_ind, ]

#Method 1: Logistic Regression
#On the first attempt, create a model that uses all of the variables.
glm.fit=glm(as.factor(effort.bin)~.,data=Data ,family=binomial, subset = train_ind)
summary(glm.fit)
glm.probs=predict(glm.fit ,test, type="response")
glm.pred=rep("Low",nrow(test))
glm.pred[glm.probs >.5]= "High"
table(glm.pred ,test$effort.bin)
mean(glm.pred!=test$effort.bin)

#On the second attempt, create a model that uses the statistically significant variables found by the first model. 
glm.fit2=glm(as.factor(effort.bin)~AdjustedFunctionPoints,data=Data ,family=binomial, subset = train_ind)
summary(glm.fit2)
glm.probs2=predict(glm.fit2 ,test, type="response")
glm.pred2=rep("Low",nrow(test))
glm.pred2[glm.probs2 >.5]= "High"
table(glm.pred2 ,test$effort.bin)
mean(glm.pred2!=test$effort.bin)

#On the third attempt, create a model that uses the 2 most statistically significant variables found by the first model. 
glm.fit3=glm(as.factor(effort.bin)~AdjustedFunctionPoints + Outputcount ,data=Data ,family=binomial, subset = train_ind)
summary(glm.fit3)
glm.probs3=predict(glm.fit3 ,test, type="response")
glm.pred3=rep("Low",nrow(test))
glm.pred3[glm.probs3 >.5]= "High"
table(glm.pred3 ,test$effort.bin)
mean(glm.pred3!=test$effort.bin)

#Using bootstrap, resample with replacement the test data and calculate the confidence intervals for the test error rate.
library(boot)
boot.fn=function (data ,index){glm.probs.boot=predict(glm.fit3 ,data[index,], type="response")
glm.pred.boot=rep("Low",nrow(data))
glm.pred.boot[glm.probs.boot >.5]= "High"
mean(glm.pred.boot!=data$effort.bin)}
btstrp = boot(test ,boot.fn ,1000)
c(btstrp$t0 - 2 * 0.03867708, btstrp$t0 + 2 * 0.03867708)
btstrp

#Method 2: Decision Tree
library(tree)
#On the first step, create a decision tree using all of the dataset's variables.
tree = tree(as.factor(effort.bin)~.,Data, subset=train_ind)
summary(tree)
plot(tree)
text(tree ,pretty =0)
tree.pred=predict(tree ,test ,type="class")
table(tree.pred ,test$effort.bin)
(51+59)/152

#On the second step, check if pruning the tree improves the error rate.
cv.effort =cv.tree(tree ,FUN=prune.misclass)
cv.effort
par(mfrow=c(1,2))
plot(cv.effort$size ,cv.effort$dev ,type="b")
plot(cv.effort$k ,cv.effort$dev ,type="b")
#A seed is set but the cv gives different results, it switches between 2 and 7 as the optimal pruning.
#2 gives the optimal results so its selected.
prune.effort =prune.misclass(tree ,best=2)
plot(prune.effort)
text(prune.effort ,pretty =0)
tree.pred=predict(prune.effort ,test , type="class")
table(tree.pred ,test$effort.bin)
(52+57)/152

#Using bootstrap, resample with replacement the test data and calculate the confidence intervals for the test error rate.
boot.fn=function (data ,index){tree.pred.boot = predict(tree, data[index,], type="class")
mean(tree.pred.boot!=data$effort.bin)}
btstrp = boot(test ,boot.fn ,1000)
c(btstrp$t0 - 2 * 0.04195706, btstrp$t0 + 2 * 0.04195706)
btstrp

#Method 3: Support Vector Machines
library(e1071)
#On the first step, create a svm model with a polynomial kernel.
svmfit=svm(as.factor(effort.bin)~., data=train, kernel ="polynomial", gamma=1, cost=1)
table(true=test$effort.bin, pred=predict(svmfit ,test))
(70+37)/152
#On the second step, check if the radial kernel perfoms better.
svmfit2=svm(as.factor(effort.bin)~., data=train, kernel ="radial", gamma=1, cost=1)
table(true=test$effort.bin, pred=predict(svmfit2 ,test))
(68+41)/152
set.seed(1)
#Using cross validation, pick the best values for gamma and cost.
tune.out=tune(svm , as.factor(effort.bin)~., data=train, kernel ="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=test$effort.bin, pred=predict(tune.out$best.model ,test))
#A seed is set but the cv gives different results, it switches between some values.
#The optimal ones after some tests is:cost=10,gamma=2
(71+42)/152

#Using bootstrap, resample with replacement the test data and calculate the confidence intervals for the test error rate.
boot.fn=function (data ,index){pred=predict(tune.out$best.model ,data[index,])
mean(pred!=data$effort.bin)}
btstrp = boot(test ,boot.fn ,1000)
c(btstrp$t0 - 2 * 0.03961849, btstrp$t0 + 2 * 0.03961849)
btstrp
