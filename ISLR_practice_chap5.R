library(ISLR)
set.seed (1)

library(boot)
glm.fit=glm(mpg~horsepower ,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
  }
cv.error

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
cv.error.10


# Chap6
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) # alpha=1 for lasso

library(pls)
set.seed (2)
pcr.fit=pcr(Salary~., data=Hitters ,scale=TRUE,
              validation ="CV")
pcr.fit
pcr.pred=predict(pcr.fit,ncomp=7)
mean((pcr.pred-Hitters$Salary)^2)


               