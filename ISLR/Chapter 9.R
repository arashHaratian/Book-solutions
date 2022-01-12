# 9.6 Lab: Support Vector Machines ---------------------

library(e1071)

## 9.6.1 Support Vector Classiﬁer --------------------------

set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] <- x[y==1,] + 1
plot(x, col = (3-y))


dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)


svmfit <- svm(y~., data=dat, kernel ="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)


set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary (tune.out)

bestmod <- tune.out$best.model
summary(bestmod)

xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

svmfit <- svm(y~., data=dat, kernel ="linear", cost=.01, scale=FALSE)
ypred <- predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)


x[y==1,] <- x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch =19)


dat <- data.frame(x=x, y=as.factor(y))

svmfit <- svm(y~., data=dat, kernel ="linear", cost=1e5) 
summary(svmfit)
plot(svmfit, dat)


svmfit <- svm(y~., data=dat , kernel ="linear", cost=1)
summary(svmfit)
plot(svmfit, dat)




## 9.6.2 Support Vector Machine -----------------------------

set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,]+ 2
x[101:150,] <- x[101:150,]-2
y <- c(rep(1,150), rep(2, 50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y)

train <- sample(200,100)

svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])

summary(svmfit)


svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])


set.seed(1)
tune.out <- tune(svm, y~., data=dat[train ,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary (tune.out)


table(true=dat[-train ,"y"], pred = predict(tune.out$best.model, newdata=dat[-train,]))


## 9.6.3 ROC Curves -------------------------------

library(ROCR)

rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")         # (, "fpr", "tpr")
  plot(perf, ...)
}

svmfit.opt <- svm(y~., data=dat[train ,], kernel ="radial", gamma=2, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.opt, dat[train,], decision.values=TRUE))$decision.values

par(mfrow=c(1,2))

rocplot(fitted, dat[train,"y"], main="Training Data")


svmfit.flex <- svm(y~., data=dat[train ,], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.flex, dat[train,], decision.values=T))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")



fitted <- attributes(predict(svmfit.opt, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train,"y"], main="Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train,], decision.values=T))$decision.values
rocplot(fitted, dat[-train,"y"], add=T, col="red")




## 9.6.4 SVM with Multiple Classes ------------------------------

set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0,50)) 
x[y==0,2] <- x[y==0,2]+2
dat <- data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

svmfit <- svm(y~., data=dat, kernel ="radial", cost=10, gamma=1)
plot(svmfit, dat)


## 9.6.5 Application to Gene Expression Data -------------------------------

library(ISLR)

names(Khan)

dim(Khan$xtrain)
length(Khan$ytrain)
dim(Khan$xtest)
length(Khan$ytest)


table(Khan$ytrain)
table(Khan$ytest)

dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear", cost=10)
summary(out)

table(out$fitted, dat$y)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)



## 9.7 Exercises -----------------------

## 5

x1 <- runif(500) - 0.5
x2 <- runif (500) -0.5
y <- 1*(x1^2-x2^2 > 0)
data <- data.frame(x1,x2,y)
plot(x1, x2, col = (y+1))


  glm.fit <- glm(y~x1 + x2, family = "binomial")
  pred <- predict(glm.fit, type = "response")
  pred <- ifelse(pred > 0.5, 1, 0)
  plot(x1, x2, col = (pred+1))


glm.fit <- glm( y~poly(x1, 2) + poly(x2, 2), data = data, family = "binomial")
pred <- predict(glm.fit, type = "response")
pred <- ifelse(pred > 0.5, 1, 0)
plot(x1, x2, col = (pred+1))


svc <- svm(y~x1+x2, kernel = "linear")
pred <- predict(svc, type = "response")
pred <- ifelse(pred > 0.5, 1, 0)
plot(x1, x2, col = (pred+1))


svm <- svm(y~x1+x2, kernel = "radial")
pred <- predict(svc, type = "response")
pred <- ifelse(pred > 0.5, 1, 0)
plot(x1, x2, col = (pred+1))


## 7

var <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpglevel <- as.factor(var)

tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(tune.out)

tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4)))
summary(tune.out)

set.seed(1)
tune.out <- tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

?plot.svm
plot(tune.out$best.model , Auto , cylinders~horsepower) 


## 8

train <- sample(1:nrow(OJ), 800)
OJ.test <- OJ[-train,]

svc <- svm(Purchase~., OJ, subset = train, kernel = "linear", cost = 0.01)
summary(svc)

pred <- predict(svc)
table(truth = OJ[train,"Purchase"], pred = pred)
pred <- predict(svc, OJ.test)
table(truth = OJ.test[,"Purchase"], pred = pred)


tune.fit <- tune(svm, Purchase~., data = OJ, kernel = "linear", ranges = list(costs = seq(from = 0.01, to = 10, length.out = 20)))
summary(tune.fit)
pred <- predict(tune.fit$best.model, OJ[train,])
table(truth = OJ[train,"Purchase"], pred = pred)
pred <- predict(tune.fit$best.model, OJ.test)
table(truth = OJ.test[,"Purchase"], pred = pred)



svm <- svm(Purchase~., OJ, subset = train, kernel = "radial", cost = 0.01)
summary(svm)

pred <- predict(svm)
table(truth = OJ[train,"Purchase"], pred = pred)
pred <- predict(svm, OJ.test)
table(truth = OJ.test[,"Purchase"], pred = pred)

tune.fit <- tune(svm, Purchase~., data = OJ, kernel = "radial", ranges = list(costs = seq(from = 0.01, to = 10, length.out = 20)))
summary(tune.fit)
pred <- predict(tune.fit$best.model, OJ[train,])
table(truth = OJ[train,"Purchase"], pred = pred)
pred <- predict(tune.fit$best.model, OJ.test)
table(truth = OJ.test[,"Purchase"], pred = pred)




svm <- svm(Purchase~., OJ, subset = train, kernel = "polynomial", cost = 0.01,  degree=2)
summary(svm)

pred <- predict(svm)
table(truth = OJ[train,"Purchase"], pred = pred)
pred <- predict(svm, OJ.test)
table(truth = OJ.test[,"Purchase"], pred = pred)

tune.fit <- tune(svm, Purchase~., data = OJ, kernel = "polynomial", degree=2, ranges = list(costs = seq(from = 0.01, to = 10, length.out = 20)))
summary(tune.fit)
pred <- predict(tune.fit$best.model, OJ[train,])
table(truth = OJ[train,"Purchase"], pred = pred)
pred <- predict(tune.fit$best.model, OJ.test)
table(truth = OJ.test[,"Purchase"], pred = pred)
