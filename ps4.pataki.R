
#Question 2

#a. generate data for three separable classes
set.seed(101)
x=matrix(rnorm(60*50),60,50)
xmean=matrix(rnorm(3,sd=1.5),3,50)
which=sample(rep(1:3,20),60,replace=FALSE)
x=x+xmean[which,]
plot(x, col=which, pch=19)

#b. perform PCA on 60 observations and plot first two PC score vectors
#Rather the 60 × 50 matrix pr.out$x has as its columns the principal component score 
#vectors. The kth column is the kth principal component score vector.
pr.out = prcomp(x, scale=TRUE)
pc.score1 = pr.out$x[,1]
pc.score2 = pr.out$x[,2]
plot(pc.score1,pc.score2,col=which, pch=19, main="PC1 and PC2 Score Vectors")

#c perform K-means clustering of the observations with K=3. How well do the clusters
#that you obtained in K-means clustering compare to the true class labels?

set.seed(100)
km.out=kmeans(x,3,nstart=20)
plot(x,col=which,pch=19)
points(x,col=km.out$cluster,cex=2,pch=1,lwd=2, main = "K-means cluster analysis: K=3")

#reassign class labels appropraitely
cluster.labels = km.out$cluster
for (i in 1:60) {
    if (cluster.labels[i]==2) {
        cluster.labels[i]=3 
    } else if (cluster.labels[i]==3) {
        cluster.labels[i]=2 
    }
}

points(x,col=cluster.labels,cex=2,pch=1,lwd=2)

#calculate a table of labels versus cluster numbers, to reveal the spread
table(data.frame(which, cluster.labels))


#d
set.seed(100)
km.out=kmeans(x,2,nstart=20)
plot(x,col=km.out$cluster,cex=2,pch=1,lwd=2, main="K-means Analysis with K=2")
points(x,col=which,pch=19)
cluster.labels = km.out$cluster
for (i in 1:60) {
    if (cluster.labels[i] ==1)  {
        cluster.labels[i]=2    
    } else if (cluster.labels[i] ==2) {
        cluster.labels[i]=1
    }      
}
points(x,col=cluster.labels,cex=2,pch=1,lwd=2)
cluster = km.out$cluster
table(data.frame(class.labels, cluster.labels))




#this k-means clustered the first group fine but assigned pretty much all the other 
#observations into the second cluster. IT's miscalssification error rate will be at
#least 33%

#e
set.seed(100)
km.out=kmeans(x,4,nstart=20)
plot(x,col=km.out$cluster,cex=2,pch=1,lwd=2, main = "K-means cluster analysis: K=4")
points(x,col=which,pch=19)
#how do you begin to try and make sense and separate these classes
cluster = km.out$cluster
class.labels = c()
for(i in 1:60) {
    if (which[i] == 1) {
        class.labels[i] = "black"
    } else if (which[i] == 2) {
        class.labels[i] = "red"
    } else if (which[i] == 3) {
        class.labels[i] = "green"
    }
}
table(data.frame(class.labels, cluster))

#f
set.seed(100)
pc.mat= cbind(pc.score1,pc.score2)
set.seed(100)
km.out=kmeans(pc.mat,3,nstart=20)
plot(x,col=which,pch=19)
points(x, col=km.out$cluster, cex=2,pch=1,lwd=2)


#calculate a table of labels versus cluster numbers, to reveal the spread
cluster.labels = km.out$cluster
table(data.frame(which, cluster.labels))


set.seed(100)
pc.mat= cbind(pc.score1,pc.score2)
set.seed(100)
km.out=kmeans(pc.mat,4,nstart=20)
cluster=km.out$cluster
table(data.frame(class.labels, cluster))


#g
set.seed(100)
scaledX = scale(x,scale=TRUE)
km.out=kmeans(scaledX,3,nstart=20)
plot(x,col=km.out$cluster,cex=2,pch=1,lwd=2)
points(x,col=which,pch=19)
points(x,col=c(1,2,3)[which],pch=19)

#calculate a table of labels versus cluster numbers, to reveal the spread
cluster.labels = km.out$cluster
table(data.frame(which, cluster.labels))



#Question 3
install.packages('randomForest')
library(randomForest)
setwd("~/Downloads/")
load("body.Rdata")

set.seed(100)
n = nrow(Y)
train = sample(n, 307) 
test = -train
body.test=Y[test ,"Weight"]

#bagging
bag.body=randomForest(Y$Weight~.,data=X,subset=train, mtry=21,importance =TRUE, xtest=X[test,], 
                      ytest=Y$Weight[test],type=regression)
train.mse.bag = bag.body$mse
test.mse.bag = bag.body$test$mse

#randomForest; following book in choosing number of predictors to use - ISLR suggests p/3 for regression 
rf.body=randomForest(Y$Weight~.,data=X,subset=train, mtry=7,importance =TRUE, xtest=X[test,],
                     ytest=Y$Weight[test],type=regression)
train.mse.rf = rf.body$mse
test.mse.rf = rf.body$test$mse

#plot test mse bagging and random forest as a function of number of trees
par(mfrow=c(1,1))
plot(test.mse.bag,type="s", col="black",ylim=c(5,30), main="Test MSE on Body Data Set", xlab="Number of Trees", ylab="Test MSE")
lines(test.mse.rf, type="s",col="orange")
legend("topright", legend=c("Test: Bagging", "Test: Random Forest"), lty=c(1,1), col=c("black", "orange"))

#b
rf.body$importance[order(-rf.body$importance[,1]), ]

#waist girth, chest girth, shoulder girth, and forearm girth are the four most important variables under random forest. 

bag.body$importance[order(-bag.body$importance[,1]), ]

#waist girth, chest girth, knee girth, and hip girth are the four most important variables under random forest. 
#The top two variables are the same under both approaches. 

mse.rf = rf.body$importance[order(-rf.body$importance[,1]), 2]
mse.bag = bag.body$importance[order(-bag.body$importance[,1]), 2]

#mean decrease in MSE of out-of-bag samples expressed relative to the maximum
mse.rf = 100*(mse.rf/max(mse.rf))
mse.rf.names = names(mse.rf)
mse.bag = 100*(mse.bag/max(mse.bag))
mse.bag.names = names(mse.bag)

par(mfrow=c(1,2))
par(mar=c(5,8,4,2))
par(las=2)

barplot(mse.rf, main="Random Forest", horiz=TRUE, names.arg=mse.rf.names, cex.names=0.8, col="red")
barplot(mse.bag, main="Bagging", horiz=TRUE, names.arg=mse.bag.names, cex.names=0.8, col="orange")


#do as one plot
#mse.rf = rf.body$importance[order(-rf.body$importance[,1]), 2]
#mse.bag = bag.body$importance[order(-bag.body$importance[,1]), 2]
#mean decrease in MSE of out-of-bag samples expressed relative to the maximum
#mse.rf = as.matrix(100*(mse.rf/max(mse.rf)))
#mse.rf.names = names(mse.rf)
#mse.bag = as.matrix(100*(mse.bag/max(mse.bag)))
#mse.bag.names = names(mse.bag)

#mse.rf = mse.rf[order(rownames(mse.rf)),]
#mse.bag = mse.bag[order(rownames(mse.bag)),]
#mse.comp = as.data.frame(cbind(mse.rf,mse.bag))
#mse.comp = mse.comp[order(-mse.comp$mse.rf),]
#mse.comp = as.matrix(mse.comp)

#par(mfrow=c(1,1))
#par(mar=c(5,8,4,2))
#par(las=2)
#mse.comp.names=names(mse.comp)
#barplot(mse.comp, main="Variable Importance", beside=T, horiz=T,cex.names=0.8, 
#        col=c("red","orange"), names.arg=mse.comp.names)

#legend("topright", c("Random Forest","Bagging"), pch=15, col=c("red","orange"), bty="n")



#c

#Backward Stepwise:     Test MSE: 9.3  (solutions 8.63 for Forward)
#PCR:                   Test MSE: 9.2  (solutions 9.27)
#PLS:                   Test MSE: 8.59 (solutions: 8.65)

#Test MSE for Random Forest with 500 trees
test.mse.rf[500]

#The test MSE for random forest is comparable with the results from the other three methods used in 
#the last problem set but it actually has the worst score in the group.

#d

min(test.mse.rf)
test.mse.rf[500]
plot(test.mse.rf[100:500],xaxt="n",type="s",xlab="Number of Trees")
axis(1, at=seq(0,400, by=25), labels=seq(100,500, by=25))

#this is all well and good but often times we don;t have test data in the world and we must choose 
#the number of trees based off of our training data

#plot the training mse for random forest
plot(test.mse.bag,type="s", col="blue",ylim=c(5,30), main="Train MSE on Body Data Set", xlab="Number of Trees", ylab="Train MSE")
lines(train.mse.rf, type="s",col="red")
legend("topright", legend=c("Train: Bagging", "Train: Random Forest"), lty=c(1,1), col=c("blue", "red"))

#It looks like there's no need to increase the number of trees since the training mse levels off after
#approximately 100 trees, but the scale is distorted due to the massive initial drop in training mse
#so I'll narrow in a bit. First, let's find our minimum in the data
min.train.mse = which(train.mse.rf==min(train.mse.rf))
train.mse.rf[min.train.mse]

plot(train.mse.rf[100:500],xaxt="n",type="s",main="Train MSE on Body Data Set",xlab="Number of Trees")
axis(1, at=seq(0,400, by=25), labels=seq(100,500, by=25))
abline(v=min.train.mse-100, col = "red", type="l", lty=3, lwd=1)

#We could also perform cross-validation to get at the right number of trees and see if this synchs
#up with our results above.

rf.body=randomForest(Y$Weight~.,data=X,subset=train, mtry=7, cross=10)
train.mse.rf = rf.body$mse
par(mfrow=c(1,1))
min.train.mse = which(train.mse.rf==min(train.mse.rf))
train.mse.rf[min.train.mse]
plot(train.mse.rf[100:500],xaxt="n",type="s",main="Train MSE on Body Data Set with CV, k=10",xlab="Number of Trees")
axis(1, at=seq(0,400, by=25), labels=seq(100,500, by=25))
abline(v=min.train.mse-100, col = "red", type="l", lty=3, lwd=1)
min.train.mse

#With cross validation we see a minimum mse of 235 trees. There doesn't seem to be much point using more than 500 trees

#Question 4
install.packages("e1071")
library(e1071)

#a
x=matrix(c(3,2,4,1,2,4,4,4,2,4,4,1,3,1),7,2)
y=rep(c(-1,1),c(4,3))
plot(x,col=y+3,pch=19)

#b
dat=data.frame(x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=10, scale=FALSE)

#make our own grid to improve on plot function
#shows linear boundary and support vectors
make.grid=function(x,n=75){
    grange=apply(x,2,range)
    x1=seq(from=grange[1,1],to=grange[2,1],length=n)
    x2=seq(from=grange[1,2],to=grange[2,2],length=n)
    expand.grid(X1=x1,X2=x2)
}

xgrid=make.grid(x)
ygrid=predict(svmfit,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,],pch=5,cex=2)

#extract coefficients and plot margins
beta=drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0=svmfit$rho# rho should be negative
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,],pch=5,cex=2)
abline(beta0/beta[2],-beta[1]/beta[2])
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)

intercept = beta0/beta[2] #svm object rho is negative intercept
slope = -beta[1]/beta[2]

intercept
slope

#equation of hyperplane: 0.5 - X1 + X2 = 0 

#c

#classification rule of the maximal margin classifier
#Classify to Red if 0.5 - X1 + X2 > 0, and 
#classify to Blue otherwise


#d

#insert hand written work


#e

#(2,1),(4,3),(2,2),(4,4)

#f

#No it would not. This is becuase the maximal margin hyperplan is based 
#upon a subset of the data points, in this case {(2,1),(4,3),(2,2),(4,4)}. 
#As long as the movement of the seventh observation was small enough that
#it did not cross the boundary set by the margin, the maximal margin hyperplane
#would not be affected

#g
``` {r q4g, echo=FALSE}
xgrid=make.grid(x)
ygrid=predict(svmfit,xgrid)
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,],pch=5,cex=2)

#extract coefficients and plot margins
beta=drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0=svmfit$rho# rho should be negative
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,],pch=5,cex=2)
abline(beta0/beta[2],-beta[1]/beta[2])
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)
intercept = beta0/beta[2] #svm object rho is negative intercept
slope = -beta[1]/beta[2]
abline(12,-4,col="3")

```



#h
plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19)
points(x[svmfit$index,],pch=5,cex=2)
abline(beta0/beta[2],-beta[1]/beta[2])
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)
points(3.5,2,col=2,pch=19)
points(3.5,2,pch=1,cex=3)

