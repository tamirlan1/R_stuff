library('ggplot2')
x = c(1,6,2)
y = c(1,4,3)
length(x)
x+y

ls()
rm(x)

?matrix
x = matrix(c(2,3,4,5), nrow = 2, ncol = 2, byrow = TRUE)
sqrt(x)
x^2
x

set.seed(2112)
x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = .1)
cor(x, y)

plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",
     main="Plot of X vs Y")
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()

setwd(dir = 'Desktop/My_stuff/R_stuff')
getwd()

x = seq(1:10)
1:10
seq(-pi, pi, length = 50)

y=x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))
contour (x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
?contour

df = read.csv('data.csv')
head(df)
dim(df)
names(df)
plot(df$CarParcPercent, df$Claims, type='l', col='red')
summary(df)
