library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
lm.fit=lm(medv~lstat, data=Boston)
lm.fit
summary(lm.fit)
names(lm.fit)
lm.fit$coefficients
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "confidence")

attach(Boston)
plot(lstat ,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues (lm.fit))
which.max(hatvalues (lm.fit))


# Multiple linear regression
lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)

lm.fit=lm(medv~.,data=Boston)
summary(lm.fit)

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lm.fit=lm(medv~lstat)
anova(lm.fit ,lm.fit2)

lm.fit5=lm(medv~poly(lstat ,5))
summary(lm.fit5)



attach(Carseats)
contrasts (ShelveLoc ) #one hot encoding


fun = function(x){
  return(x^2)
}
fun(22)



glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial)
summary(glm.fits)
coef(glm.fits)
glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Smarket$Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"
glm.pred

attach(Smarket)
train=(Year <2005)
Smarket.2005 = Smarket [! train ,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume , data=Smarket ,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs >.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket ,subset=train)
qda.fit


library(class)
train.X=cbind(Lag1 ,Lag2)[train ,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction =Direction [train]
set.seed (1)
knn.pred=knn(train.X,test.X,train.Direction ,k=1)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
knn.pred=knn(train.X,test.X,train.Direction ,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)
?sample
