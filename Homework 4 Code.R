library(ISLR)
head(Auto)

#1a
summary(lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+
             origin,data=Auto))

#1b
summary(lm(mpg~weight+year+origin, data=Auto))

#1c
Auto$mpglog=log(Auto$mpg)
summary(lm(mpglog~cylinders+displacement+horsepower+weight+acceleration+year+
             origin, data=Auto))

#1d
summary(lm(mpglog~cylinders+horsepower+weight+year+origin, data=Auto))
summary(lm(mpglog~horsepower+year+weight+origin,data=Auto))

#------------Q3 Below------------------------------
HM4.test.2022 <- read.table("C:/Users/af44m/OneDrive/Desktop/Analytics/HM4-test-2022.txt",row.names=1, sep="")
HM4.train.2022 <- read.table("C:/Users/af44m/OneDrive/Desktop/Analytics/HM4-train-2022.txt", row.names=1, sep="")

table1=HM4.test.2022
table1$y=as.factor(table1$y)
table2=HM4.train.2022
table2$y=as.factor(table2$y)


#3a
problem3a=glm(y~., data=table2, family=binomial)
#summary(problem3a)
predict.test1=predict(problem3a, table1, type="response" )
#predict.test1
test.response1=rep("A", nrow(table1))
test.response1[predict.test1>0.5]<-"B"
table(test.response1, table1$y) 
mean(test.response1!=table1$y) 


#3b
install.packages("MASS")
install.packages("car")
install.packages("caret")
install.packages("klaR")
install.packages("rattle.data")

library(MASS)

problem3b=lda(y~., data=table2)
problem3b
predict.test2=predict(problem3b, table1, type="response" )
table(predict.test2$class,table1$y) 
mean(predict.test2$class!=table1$y) 


#3c
problem3c=qda(y~., data=table2)
problem3c
predict.test3=predict(problem3c, table1, type="response" )
table(predict.test3$class, table1$y) 
mean(predict.test3$class!=table1$y) 


#3d
install.packages("class")

library(class)
set.seed(2022)
pred=knn(cbind(table2$x.1, table2$x.2),cbind(table1$x.1, table1$x.2) ,table2$y, k=2)
table(pred,table1$y)
mean(pred!=table1$y)

