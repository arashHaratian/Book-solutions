## 6.5 Lab 1: Subset Selection Methods -------------------------------------

## 6.5.1 Best Subset Selection -----------------------------

library(ISLR)

?Hitters
dim(Hitters)

sum(is.na(Hitters$Salary))


Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

library(leaps)

?regsubsets


regfit.full <- regsubsets(Salary~., Hitters)

summary(regfit.full)

regfit.full <- regsubsets(Salary~., Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="adg- R2", type="l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col="red", cex = 2, pch=20)

plot(reg.summary$cp, xlab="Number of Variables", ylab="cp", type="l")
plot(reg.summary$bic, xlab="Number of Variables", ylab="bic", type="l")

which.min(reg.summary$cp)
which.min(reg.summary$bic)
which.min(reg.summary$rss)

?plot.regsubsets

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

names(reg.summary)
coef(regfit.full, 6) ##bic ## the number indicate that how many variable used in a model


regfit.fwd <- regsubsets(Salary~., Hitters, method = "forward", nvmax = 19)
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~., Hitters, method = "backward", nvmax = 19)
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


## 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation -----------------------

set.seed(1)

train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
test <- !train


regfit.best <- regsubsets(Salary~., Hitters[train,], nvmax = 19)

test.mat <- model.matrix(Salary~., data = Hitters[test,])


val.errors <- vector("double", length = 19)
for(i in 1:19){
  coefi <- coef(regfit.best, i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

val.errors
which.min(val.errors)
plot(val.errors)


predict.regsubsets <- function(object, newdata, id){
  form <- as.formula (object$call[[2]]) 
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}


k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace=TRUE) 
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))


for(j in 1:k){
  best.fit <- regsubsets(Salary~., Hitters[folds != j,], nvmax = 19)
  for (i in 1:19){
    pred <- predict.regsubsets(best.fit, Hitters[folds == j,], id = i)
    cv.errors[j, i] <- mean((Hitters$Salary [folds==j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
plot(mean.cv.errors, type="b")



regfit.best <- regsubsets(Salary~., Hitters, nvmax = 19)
coef(regfit.best, 10)

## 6.6 Lab 2: Ridge Regression and the Lasso --------------------------

library(glmnet)
Hitters <- na.omit(Hitters)

x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary


## 6.6.1 Ridge Regression ---------------------------------

library(glmnet)
grid <- 10 ^ seq(10, -2, length =100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid) ## for truning off the auto standardise "standardize=FALSE"
dim(coef(ridge.mod))

ridge.mod$lambda [50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
#--
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))


predict(ridge.mod, s=50, type="coefficients")[1:20,]

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]


ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test ,])
mean((ridge.pred - y.test)^2)

mean((mean(y[train])-y.test)^2)   ## fiting just with intercept term
#--
ridge.pred <- predict(ridge.mod ,s=1e10, newx=x[test,])
mean((ridge.pred -y.test)^2)


ridge.pred <- predict(ridge.mod, s=0, newx=x[test, ], exact=T)
mean((ridge.pred -y.test)^2)

lm(y~x, subset=train)
predict (ridge.mod ,s=0,exact=T,type="coefficients")[1:20,]


cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min 
bestlam

ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,]) 
mean((ridge.pred -y.test)^2) 
out <- glmnet(x, y, alpha = 0)
predict(out, type="coefficients", s=bestlam)[1:20,]


## 6.6.2 The Lasso ---------------------------------

lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict (lasso.mod ,s=bestlam ,newx=x[test,])
mean((lasso.pred -y.test)^2) 

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,] 
lasso.coef
lasso.coef[lasso.coef!=0]


# 6.7 Lab 3: PCR and PLS Regression ----------------------------

# 6.7.1 Principal Components Regression ----------------------------------

library(pls)
  
set.seed(2)
pcr.fit <- pcr(Salary~., data = Hitters, scale=TRUE, validation="CV")
summary(pcr.fit) 
plot(pcr.fit)

validationplot(pcr.fit, val.type="MSEP")

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred <- predict (pcr.fit ,x[test ,],ncomp = 7)
mean((pcr.pred -y.test)^2)

pcr.fit <- pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)


## 6.7.2 Partial Least Squares -----------------

set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)


pls.pred <- predict(pls.fit, x[test ,], ncomp = 2)
mean((pls.pred - y.test)^2)

pls.fit <- plsr(Salary~., data=Hitters, scale=TRUE, ncomp = 2)
summary(pls.fit)


## 6.8 Exercises ---------------------------------


## 8

library(leaps)
x <- rnorm(100)
eps <- rnorm(100)


y <- 2 + 4 * x - 3.34 * x^2 + 0.73 * x^3 + eps

data <- data.frame(x,y)
bestsub <- regsubsets(y ~ poly(x, 10), data, nvmax = 10)
bestsub.summary <- summary(bestsub)
names(bestsub.summary)

par(mfrow = c(2,2))
plot(bestsub.summary$rss, type =  "l")
plot(bestsub.summary$adjr2, type =  "l")
points(which.max(bestsub.summary$adjr2), )
plot(bestsub.summary$cp, type =  "l")
points(which.min(bestsub.summary$cp))
plot(bestsub.summary$bic, type =  "l")
points(which.min(bestsub.summary$bic))

coef(bestsub, id = 3)

##--

bestsub <- regsubsets(y ~ poly(x, 10), data, nvmax = 10, method = "forward")
bestsub.summary <- summary(bestsub)
names(bestsub.summary)

par(mfrow = c(2,2))
plot(bestsub.summary$rss, type =  "l")
plot(bestsub.summary$adjr2, type =  "l")
points(which.max(bestsub.summary$adjr2))
plot(bestsub.summary$cp, type =  "l")
points(which.min(bestsub.summary$cp))
plot(bestsub.summary$bic, type =  "l")
points(which.min(bestsub.summary$bic))

coef(bestsub, id = 3)



library(glmnet)
data.mat <- model.matrix(y~poly(x, 10, raw = T), data)
lasso <- cv.glmnet(x = data.mat[, -1], y, alpha = 1)
plot(lasso)

lasso$lambda.min

predict(lasso, s=lasso$lambda.min, type="coefficients")
predict(lasso, s=1, type="coefficients")



y <- 2 -12 * x^7 + eps

data <- data.frame(x,y)
bestsub <- regsubsets(y ~ poly(x, 10), data, nvmax = 10)
bestsub.summary <- summary(bestsub)
names(bestsub.summary)

par(mfrow = c(2,2))
plot(bestsub.summary$rss, type =  "l")
plot(bestsub.summary$adjr2, type =  "l")
points(which.max(bestsub.summary$adjr2))
plot(bestsub.summary$cp, type =  "l")
points(which.min(bestsub.summary$cp))
plot(bestsub.summary$bic, type =  "l")
points(which.min(bestsub.summary$bic))

#--

data.mat <- model.matrix(y~poly(x, 10, raw = T), data)
lasso <- cv.glmnet(x = data.mat[, -1], y, alpha = 1)
plot(lasso)

lasso$lambda.min

predict(lasso, s=lasso$lambda.min, type="coefficients")


## 9

library(ISLR)

set.seed(1)
trainid <- sample(1:nrow(College), nrow(College)/2)
College.train <- College[trainid, ]
College.test <- College[-trainid, ]

lm.fit <- lm(Apps~., College.train)
lm.pred <- predict(lm.fit, College.test)
mean((College.test$Apps - lm.pred )^2)


ridge.fit <- cv.glmnet(model.matrix(Apps~., College.train)[, -1], College.train$Apps, alpha = 0)
plot(ridge.fit)
ridge.fit$lambda.min

ridge.pred <- predict(ridge.fit, s = ridge.fit$lambda.min, newx = model.matrix(Apps~., College.test)[, -1])
mean((College.test$Apps - ridge.pred )^2)



lasso.fit <- cv.glmnet(model.matrix(Apps~., College.train)[, -1], College.train$Apps, alpha = 1)
plot(lasso.fit)
lasso.fit$lambda.min

lasso.pred <- predict(lasso.fit, s = lasso.fit$lambda.min, newx = model.matrix(Apps~., College.test)[, -1])
mean((College.test$Apps - lasso.pred )^2)
predict(lasso.fit,  type="coefficients", s=lasso.fit$lambda.min)[-1,]



library(pls)

pcr.fit <- pcr(Apps~., data = College.train, scale = T, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, "MSEP")

pcr.pred <- predict(pcr.fit, ncomp = 15, newdata = College.test)
mean((College.test$Apps - pcr.pred )^2)


plsr.fit <- plsr(Apps~., data = College.train, scale = T, validation = "CV")
summary(plsr.fit)
validationplot(plsr.fit, "MSEP")
plsr.pred <- predict(plsr.fit, ncomp = 6, newdata = College.test)
mean((College.test$Apps - plsr.pred )^2)


## 10

x <- rnorm(20000)
x <- matrix(rnorm(20000), 1000, 20)
beta <- rnorm(20)
eps <- rnorm(1000)
beta[c(13,7,8,10)] <- 0

y <- x %*% beta + eps

train <- sample(1:1000, 100)
data <- data.frame(x,y)
data.train <- data[train,]
data.test <- data[-train,]

data.bestsub <- regsubsets(y~., data.train, nvmax = 20)
data.bestsub.summary <- summary(data.bestsub)

plot(data.bestsub.summary$rss)
plot(data.bestsub.summary$adjr2)
plot(data.bestsub.summary$cp)
plot(data.bestsub.summary$bic)

predict.regsubsets <- function(object, newdata, id){
  form <- as.formula (object$call[[2]]) 
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}


err.full <- rep(NA, 20)
for(i in 1:20) {
  pred.full <- predict(data.bestsub, data.train, id=i)
  err.full[i] <- mean((data.train$y - pred.full)^2)
}
      
plot(err.full)


err.full <- rep(NA, 20)
for(i in 1:20) {
  pred.full <- predict(data.bestsub, data.test, id=i)
  err.full[i] <- mean((data.test$y - pred.full)^2)
}
plot(err.full)
which.min(err.full)

coef(data.bestsub, id = 14)


eq <- vector("double", 20)
names(beta) <- names(coef(data.bestsub, id = 20)[-1])
for(r in 1:20){
  coefi <- coef(data.bestsub, id = r)
  s <- sum((beta - coefi[names(beta)== names(coefi)])^2)
  eq[r] <- sqrt(s)
}
plot(eq)





## 11

library(MASS)

?Boston
dim(Boston)

train <- sample(1:nrow(Boston), 400)

Boston.train <- Boston[train,]
Boston.test <- Boston[-train,]


#best selection

bestsub.summary <- summary(bestsub.fit)

plot(bestsub.summary$cp)
plot(bestsub.summary$bic)
plot(bestsub.summary$adjr2)



predict.regsubsets <- function(object, newdata, id){
  form <- as.formula (object$call[[2]]) 
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}


k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Boston), replace=TRUE) 
cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))


for(j in 1:k){
  best.fit <- regsubsets(crim~., Boston[folds != j,], nvmax = 13)
  for (i in 1:13){
    pred <- predict.regsubsets(best.fit, Boston[folds == j,], id = i)
    cv.errors[j, i] <- mean((Boston$crim [folds==j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
plot(mean.cv.errors, type="b")

bestsub.fit <- regsubsets(crim~., Boston.train, nvmax = 13)
bestsub.pred <- predict.regsubsets(bestsub.fit, Boston.test, 12)
bestsub.acc <- mean((bestsub.pred - Boston.test$crim)^2)




#lasso

lasso.fit <- cv.glmnet(model.matrix(crim~., Boston.train)[,-1], Boston.train$crim, nfolds = 10, alpha = 1)
plot(lasso.fit)
lasso.fit$lambda.min

predict(lasso.fit, s = lasso.fit$lambda.min, type = "coefficient")
lasso.pred <- predict(lasso.fit, s = lasso.fit$lambda.min, newx = model.matrix(crim~., Boston.test)[,-1])
lasso.acc <- mean((lasso.pred - Boston.test$crim)^2)




#ridge

ridge.fit <- cv.glmnet(model.matrix(crim~., Boston.train)[,-1], Boston.train$crim, nfolds = 10, alpha = 0)
plot(ridge.fit)
ridge.fit$lambda.min

predict(ridge.fit, s = ridge.fit$lambda.min, type = "coefficient")
ridge.pred <- predict(ridge.fit, s = ridge.fit$lambda.min, newx = model.matrix(crim~., Boston.test)[,-1])
ridge.acc <- mean((ridge.pred - Boston.test$crim)^2)



#pls

plsr.fit <- plsr(crim~., data = Boston.train, scale = T, validation = "CV")
summary(plsr.fit)
validationplot(plsr.fit, "MSEP")

plsr.pred <- predict(plsr.fit, ncomp = 8, newdata = Boston.test)
plsr.acc <- mean((plsr.pred - Boston.test$crim)^2)





#pcr

pcr.fit <- pcr(crim~., data = Boston.train, scale = T, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, "MSEP")

pcr.pred <- predict(pcr.fit, ncomp = 8, newdata = Boston.test)
pcr.acc <- mean((pcr.pred - Boston.test$crim)^2)

