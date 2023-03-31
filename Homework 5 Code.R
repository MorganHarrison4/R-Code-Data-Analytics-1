DataHM5 =read.table("C:/Users/af44m/OneDrive/Desktop/HM5.txt")
head(DataHM5)
model1=lm(y~., data=DataHM5)
summary(model1)



#Question 2 Part A
library(leaps)
subset1y=regsubsets(y~., data=DataHM5, nvmax=10)
summary(subset1y)


#CP Set Up
summary(subset1y)$cp
plot(summary(subset1y)$cp, type="l", main="CP vs Index Subset", ylab="CP")
points(seq(1,10), summary(subset1y)$cp, pch=16)
points(which.min(summary(subset1y)$cp), summary(subset1y)$cp
       [which.min(summary(subset1y)$cp)], col="green", pch=16)
subset1yCP=lm(y~x2+x3+x7+x8+x10, data=DataHM5)
summary(subset1yCP)



#BIC Set Up
summary(subset1y)$bic
plot(summary(subset1y)$bic, type="l", main="BIC vs Index Subset", ylab="BIC")
points(seq(1,10), summary(subset1y)$bic, pch=16)
points(which.min(summary(subset1y)$bic), summary(subset1y)$bic
       [which.min(summary(subset1y)$bic)], col="green", pch=16)
subset1yBIC=lm(y~x1+x2, data=DataHM5)
summary(subset1yBIC)


#R2 Set Up
summary(subset1y)$adjr2
plot(summary(subset1y)$adjr2, type="l", main="R2 vs Index Subset Selection", ylab="R2")
points(seq(1,10), summary(subset1y)$adjr2, pch=16)
points(which.max(summary(subset1y)$adjr2), summary(subset1y)$adjr2
       [which.max(summary(subset1y)$adjr2)], col="green", pch=16)
subset1yr2=lm(y~x1+x2+x3+x5+x7+x9, data=DataHM5)
summary(subset1yr2)
coef(subset1y, which.max(summary(subset1y)$adjr2))




#Question 2 Part B
forward1y=regsubsets(y~., data=DataHM5, nvmax=10, method="forward")
summary(forward1y)


#CP Set Up
summary(forward1y)$cp
plot(summary(forward1y)$cp, type="l", main="CP vs Index Forward ", ylab="CP")
points(seq(1,10), summary(forward1y)$cp, pch=16)
points(which.min(summary(forward1y)$cp), summary(forward1y)$cp
       [which.min(summary(forward1y)$cp)], col="green", pch=16)
coef(forward1y, which.min(summary(forward1y)$cp))


#BIC Set Up
summary(forward1y)$bic
plot(summary(forward1y)$bic, type="l", main="BIC vs Index Forward", ylab="BIC")
points(seq(1,10), summary(forward1y)$bic, pch=16)
points(which.min(summary(forward1y)$bic), summary(forward1y)$bic
       [which.min(summary(forward1y)$bic)], col="green", pch=16)
coef(forward1y, which.min(summary(forward1y)$bic))


#R2 Set Up
summary(forward1y)$adjr2
plot(summary(forward1y)$adjr2, type="l", main="R2 vs Index Subset", ylab="R2")
points(seq(1,10), summary(forward1y)$adjr2, pch=16)
points(which.max(summary(forward1y)$adjr2), summary(forward1y)$adjr2
       [which.max(summary(forward1y)$adjr2)], col="green", pch=16)
coef(forward1y, which.max(summary(forward1y)$adjr2))



#Question 2 Part C
backward1y=regsubsets(y~., data=DataHM5, nvmax=10, method="backward")
summary(backward1y)


#CP Set Up
summary(backward1y)$cp
plot(summary(backward1y)$cp, type="l", main="CP vs Index Backward Stepwise", ylab="CP")
points(seq(1,10), summary(backward1y)$cp, pch=16)
points(which.min(summary(backward1y)$cp), summary(backward1y)$cp
       [which.min(summary(backward1y)$cp)], col="green", pch=16)
coef(backward1y, which.min(summary(backward1y)$cp))


#BIC Set Up
summary(backward1y)$bic
plot(summary(backward1y)$bic, type="l", main="BIC vs Index Backward Stepwise", ylab="BIC")
points(seq(1,10), summary(backward1y)$bic, pch=16)
points(which.min(summary(backward1y)$bic), summary(backward1y)$bic
       [which.min(summary(backward1y)$bic)], col="green", pch=16)
coef(backward1y, which.min(summary(backward1y)$bic))


#R2 Set Up
summary(backward1y)$adjr2
plot(summary(backward1y)$adjr2, type="l", main="R2 vs Index Backward Stepwise", ylab="R2")
points(seq(1,10), summary(backward1y)$adjr2, pch=16)
points(which.max(summary(backward1y)$adjr2), summary(backward1y)$adjr2
       [which.max(summary(backward1y)$adjr2)],col="green", pch=16)
coef(backward1y, which.max(summary(backward1y)$adjr2))

