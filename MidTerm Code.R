data.train2022=read.table("C:/Users/af44m/OneDrive/Desktop/data.train2022.txt")
data.test2022=read.table("C:/Users/af44m/OneDrive/Desktop/data.test2022.txt")

library(MASS) 
library(class) 

train.table=data.train2022
train.table$failure=as.factor(train.table$failure)
test.table=data.test2022
test.table$failure=as.factor(test.table$failure)



#Question 1 Midterm Code-----------------------------------------------------

boxplot(temp ~ failure, data = train.table, cex.lab=1.5, cex.axis=1.5, cex.main=1.5,
        cex.sub=1.5)



#Question 2 Midterm Code-----------------------------------------------------

train.lda=lda(failure ~ ., data=train.table)
train.lda
predict.train=predict(train.lda, train.table, type="response")
table(predict.train$class, train.table$failure)
mean(predict.train$class!=train.table$failure)

test.lda=lda(failure ~ ., data=train.table)
predict.test=predict(test.lda, test.table, type="response")
table(predict.test$class, test.table$failure)
mean(predict.test$class!=test.table$failure)



#Question 3 Midterm Code-----------------------------------------------------

train.qda=qda(failure ~ ., data=train.table)
train.qda
predict.train.qda=predict(train.qda, train.table, type="response")
table(predict.train.qda$class, train.table$failure)
mean(predict.train.qda$class!=train.table$failure)

test.qda=qda(failure ~ ., data=train.table)
predict.test.qda=predict(test.qda, test.table, type="response")
table(predict.test.qda$class, test.table$failure)
mean(predict.test.qda$class!=test.table$failure)



#Question 4 Midterm Code-----------------------------------------------------

train.glm=glm(failure~., data=train.table, family = binomial)
summary(train.glm)
predict.train.glm=predict(train.glm, train.table, type="response")
train.reponse=rep("N", nrow(train.table))
train.reponse[predict.train.glm>0.5]<-"Y"
table(train.reponse,train.table$failure)
mean(train.reponse!=train.table$failure)

test.glm=glm(failure~., data=train.table, family = binomial)
predict.test.glm=predict(test.glm, test.table, type="response")
test.reponse=rep("N", nrow(test.table))
test.reponse[predict.test.glm>0.5]<-"Y"
table(test.reponse,test.table$failure)
mean(test.reponse!=test.table$failure)


#Question 5 Midterm Code-----------------------------------------------------

set.seed(2022)
predict.train.knn1=knn(cbind(train.table$temp,train.table$stress,train.table$material),
                      cbind(train.table$temp,train.table$stress,train.table$material),
                      train.table$failure, k=2)
table(predict.train.knn1, train.table$failure)
mean(predict.train.knn1!=train.table$failure)

set.seed(2022)
predict.test.knn1=knn(cbind(train.table$temp,train.table$stress,train.table$material), 
                      cbind(test.table$temp,test.table$stress,test.table$material), 
                      train.table$failure,k=2)
table(predict.test.knn1,test.table$failure)
mean(predict.test.knn1!=test.table$failure)
 

#Question 6 Midterm Code-----------------------------------------------------

set.seed(2022)
predict.train.knn2=knn(cbind(train.table$temp,train.table$stress,train.table$material),
                       cbind(train.table$temp,train.table$stress,train.table$material),
                       train.table$failure, k=7)
table(predict.train.knn2, train.table$failure)
mean(predict.train.knn2!=train.table$failure)

set.seed(2022)
predict.test.knn2=knn(cbind(train.table$temp,train.table$stress,train.table$material), 
                      cbind(test.table$temp,test.table$stress,test.table$material), 
                      train.table$failure,k=7)
table(predict.test.knn2,test.table$failure)
mean(predict.test.knn2!=test.table$failure)



#Question 7 Midterm Code-----------------------------------------------------

train.MLR = train.table
test.MLR = test.table

train.MLR$failure=rep("0",200)
train.MLR$failure[train.table$failure=="Y"]="1"
test.MLR$failure=rep("0",300) 
test.MLR$failure[test.table$failure=="Y"]="1"
trainlm=lm(failure~., data=train.MLR)
summary(trainlm)
predict.train.lm=predict(trainlm, newdata=train.MLR, type="response")
response.train.lm=rep("N",200)
response.train.lm[predict.train.lm>0.5]="Y"
table(response.train.lm,train.table$failure) 
mean(response.train.lm!=train.table$failure)


predict.test.lm=predict(trainlm, newdata=test.MLR, type="response")
response.test.lm=rep("N",300)
response.test.lm[predict.test.lm>0.5]="Y"
table(response.test.lm,test.table$failure) 
mean(response.test.lm!=test.table$failure) 















