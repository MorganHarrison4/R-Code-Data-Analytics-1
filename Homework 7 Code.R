#Problem 2

x1=c(1,1,0,5,6,4)
x2=c(4,3,4,1,2,0)
Data1=cbind(x1,x2)


#Part A
plot(Data1, pch=16, main="Plot X1 vs. X2")

#Part B
Cluster1B=cbind(c(1,1),c(4,3))
Cluster2B=cbind(c(0,5,6,4),c(4,1,2,0))
points(Cluster1B, col="red", pch=16)
points(Cluster2B, col="green", pch=16)

#Part C
Cluster1C=cbind(c(1,1,0), c(4,3,4))
Cluster2C=cbind(c(5,6,4), c(1,2,0))
plot(Cluster1C, pch=16, main="Plot X1 vs. X2", col="red", xlim=c(0,6), 
     ylim=c(0,4), xlab="x1", ylab="x2")
points(Cluster2C, col="green", pch=16)
