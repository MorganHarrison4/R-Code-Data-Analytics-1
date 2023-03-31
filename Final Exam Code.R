#Question 1---------------------------------------------------------------------
library(leaps)
train.y1=regsubsets(y~.,data = train.data.2022,nvmax=13)
summary(train.y1)
which.min(summary(train.y1)$bic)
train.best.model1=lm(y~x8+x12,data = train.data.2022)
train.predict1=predict(train.best.model1,newdata=train.data.2022)
train.y1=mean((train.data.2022$y-train.predict1)^2)
train.y1
test.predict1=predict.lm(train.best.model1,newdata = test.data.2022)
test.y1=mean((test.predict1-test.data.2022$y)^2)
test.y1

#Question 2---------------------------------------------------------------------
backward.y2=regsubsets(y~.,data=train.data.2022,nvmax = 13,method = "backward")
summary(backward.y2)

#Question 3---------------------------------------------------------------------
library(glmnet)
train.x3=model.matrix(y~.,train.data.2022)
train.x3=train.x3[,-1]
train.y3=train.data.2022$y
test.x3=model.matrix(y~.,test.data.2022)
test.x3=test.x3[,-1]
test.y3=test.data.2022$y
train.x.scale3=scale(train.x3)
test.x.scale3=scale(test.x3)
set.seed(2022)
grid=10^seq(-2,5,0.1)
ridge.cv.train3=cv.glmnet(train.x.scale3,train.y3,alpha=0)
ridge.train.model3=glmnet(train.x.scale3,train.y3,alpha=0,thresh = 1e-12,lambda=grid)
lambda.best3=ridge.cv.train3$lambda.1se
lambda.best3
predict.ridge.train3=predict(ridge.train.model3,newx=train.x.scale3,s=lambda.best3)
predict.ridge.test3=predict(ridge.train.model3,newx=test.x.scale3,s=lambda.best3)
predict(ridge.train.model3,s=lambda.best3,type="coefficients")
mean((predict.ridge.train3-train.y3)^2)
mean((predict.ridge.test3-test.y3)^2)

#Question 4---------------------------------------------------------------------
library(glmnet)
train.x4=model.matrix(y~.,train.data.2022)
train.x4=train.x4[,-1]
train.y4=train.data.2022$y
test.x4=model.matrix(y~.,test.data.2022)
test.x4=test.x4[,-1]
test.y4=test.data.2022$y
train.x.scale4=scale(train.x4)
test.x.scale4=scale(test.x4)
set.seed(2022)
grid=10^seq(-2,5,0.1)
Lasso.cv.train4=cv.glmnet(train.x.scale4,train.y4,alpha=1)
Lasso.train.model4=glmnet(train.x.scale4,train.y4,alpha=1,thresh = 1e-12,lambda=grid)
lambda.best4=Lasso.cv.train4$lambda.1se
lambda.best4
predict.Lasso.train4=predict(Lasso.train.model4,newx=train.x.scale4,s=lambda.best4)
predict.Lasso.test4=predict(Lasso.train.model4,newx=test.x.scale4,s=lambda.best4)
predict(Lasso.train.model4,s=lambda.best4,type="coefficients")
mean((predict.Lasso.train4-train.y4)^2)
mean((predict.Lasso.test4-test.y4)^2)

#Question 5---------------------------------------------------------------------
library(tree)
set.seed(2022)
train.tree5=tree(y~.,data=train.data.2022)
predict.train.tree5=predict(train.tree5,train.data.2022)
predict.test.tree5=predict(train.tree5,newdata=test.data.2022)
mean((predict.train.tree5-train.data.2022$y)^2)
mean((predict.test.tree5-test.data.2022$y)^2)

#Question 7---------------------------------------------------------------------
obj=prcomp(train.data.2022[,-1],scale=TRUE,center=TRUE)
names(obj)
summary(obj)
biplot(obj,scale=0)
pve=obj$sdev^2/sum(obj$sdev^2)
plot(pve,xlab="Number of PCs", ylab="PVE",ylim=c(0,1),type='b')

#Question 8---------------------------------------------------------------------
train.y8=train.data.2022[,-4:-14]
train.y8=train.y8[,-1]
x1.y=train.data.2022$x1
x2.y=train.data.2022$x2
set.seed(2022)
objective.train=kmeans(train.y8,centers=3,nstart=50)
#objective.train$cluster
objective.train$centers
plot(x1.y,x2.y,pch=5)
points(objective.train$centers,col="red",pch=7)

