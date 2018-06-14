library(ISLR)
attach(Wage)

#Polynomial Regression and Step Functions
fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit )

par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))
plot(age,wage,xlim=agelims ,cex=.5,col="darkgrey")
title("Degree -4 Polynomial ",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)


# CHAPTER 8
library(randomForest)
library(MASS)
set.seed (1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,
                          mtry=13,importance =TRUE)
bag.boston

yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat.bag, boston.test)
abline (0 ,1)
mean((yhat.bag-boston.test)^2)
importance (bag.boston)

library(gbm)
set.seed (1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution=
                     "gaussian" ,n.trees=5000, interaction.depth=4)
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)

boost.boston=gbm(medv~.,data=Boston[train,],distribution= "gaussian",n.trees=5000, interaction.depth=4,shrinkage =0.2, verbose =F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)
