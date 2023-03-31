data1=read.table("C:/Users/af44m/OneDrive/Desktop/HM5.txt")
data1y=data1$y
data1x=data.matrix(data[,-1])
library(glmnet)



#Question 1 Part A
grid=10^(seq(-2,5,0.1))
ridge.model1=glmnet(data1x, data1y, alpha=0,lambda=grid,thresh=1e-12)
set.seed(2)
ridge.cv=cv.glmnet(data1x, data1y, alpha=0)
lambda1=ridge.cv$lambda.1se
r.cv=predict(ridge.model1, s=lambda1, newx=data1x)
predict(ridge.model1,s=lambda1,type="coefficients")[1:11,]



#Question 1 Part B
lassomodel1=glmnet(data1x, data1y, alpha=1,lambda=grid, thresh=1e-12)
set.seed(2)
l.cv=cv.glmnet(data1x, data1y, alpha=1)
lambda1.lasso=l.cv$lambda.1se
lambda1.lasso
l.p.cv=predict(lassomodel1, s=lambda1.lasso, newx=data1x)
l.c= predict(lassomodel1, type="coefficients", s=lambda1.lasso)[1:11,]
l.c
l.c[l.c!=0]



#Question 3
library(datasets)
data2=iris[,-5]
names(data2)
iris1.p=prcomp(data2, center=TRUE, scale=TRUE)
iris1.p
biplot(iris1.p)
iris1.p$sdev^2 
p=iris1.p$sdev^2/sum(iris1.p$sdev^2)
summary(iris1.p)
plot(p,xlab="# of PC's", ylab="Proportion of Variance", ylim=c(0,1), type='b')