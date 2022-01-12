## 7.8 Lab: Non-linear Modeling ----------------------

library(ISLR)


## 7.8.1 Polynomial Regression and Step Functions ---------------------------

fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))

fit2 <- lm(wage ~ poly(age, 4, raw = T), data = Wage)
coef(summary(fit2))


fit2a <- lm(wage~age+I(age^2)+I(age^3)+I(age^4), data = Wage) 
coef(fit2a)

fit2b <- lm(wage~cbind(age, age^2, age^3, age^4), data=Wage)  
coef(fit2b)


agelims <- range(Wage$age)
age.grid <- seq(from = agelims[1], to = agelims[2])

preds <- predict(fit, newdata = list(age = age.grid), se.fit = T)

se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)


par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0)) 

plot(Wage$age, Wage$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer=T) 
lines(age.grid, preds$fit, lwd = 2, col="blue")
matlines(age.grid, se.bands,lwd=1, col="blue", lty=3)

preds2 <- predict(fit2, newdata = list(age = age.grid), se=TRUE) 
max(abs(preds$fit - preds2$fit))



fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)


anova(fit.1, fit.2, fit.3, fit.4, fit.5)


coef(summary (fit.5)) ## p-values are the same for both anova and fit.5 <----->  also (t-val)^2 = F-stat


fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)

anova(fit.1, fit.2, fit.3)


## logistic regression with poly terms

fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

preds <- predict(fit, newdata = list(age = age.grid), se=T)


pfit <- exp(preds$fit)/(1+exp(preds$fit))

se.bands.logit <- cbind(preds$fit + 2* preds$se.fit , preds$fit - 2* preds$se.fit) 
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

preds <- predict(fit, newdata = list(age = age.grid), type="response", se=T)

plot(Wage$age, I(Wage$wage>250), xlim = agelims, type="n", ylim = c(0, .2)) 

points(jitter(Wage$age), I((Wage$wage >250)/5),cex=.5,pch="|", col="darkgrey") 
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

## step function
table(cut(Wage$age, 4))
fit <- lm(wage ~ cut($age, 4), data = Wage)
summary(fit)


## 7.8.2 Splines ---------------

library(splines)

fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se=T)
plot(Wage$age, Wage$wage, col = "grey")
lines(age.grid ,pred$fit ,lwd=2)
lines(age.grid ,pred$fit +2*pred$se ,lty="dashed")
lines(age.grid ,pred$fit -2*pred$se ,lty="dashed")

## basisfunction for spine
dim(bs(Wage$age, knots = c(25, 40, 60)))
dim(bs(Wage$age, df = 6))
attr(bs(Wage$age, df = 6, degree = 3), "knots")

## natrual spline
fit2 <- lm(wage ~ ns(age, df=4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)


## smoothing spline
plot(Wage$age ,Wage$wage ,xlim = agelims ,cex = .5,col = "darkgrey ")
title("Smoothing Spline")
fit <- smooth.spline(Wage$age ,Wage$wage ,df = 16)
fit2 <- smooth.spline(Wage$age ,Wage$wage ,cv = TRUE)
fit2$df
lines(fit ,col="red",lwd=2)
lines(fit2 ,col="blue",lwd=2)
legend ("topright ",legend=c("16 DF","6.8 DF"), col=c("red","blue"),lty=1,lwd=2,cex=.8)



## local regression
plot(Wage$age, Wage$wage, xlim=agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage) ## also we can use locfit() 
fit2=loess(wage~age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col="red", lwd=2)
lines(age.grid ,predict (fit2, data.frame(age = age.grid)), col="blue", lwd=2)
legend ("topright", legend = c("Span=0.2", "Span=0.5"), col=c("red", "blue"), lty = 1, lwd = 2, cex = .8)



## 7.8.3 GAMs ----------------------

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)

## smoothing spline GAM
library(gam)

gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

plot.Gam(gam1, se=TRUE, col="red")


gam.m1 <- gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)


anova(gam.m1, gam.m2, gam.m3, test = "F") 

summary(gam.m3)


preds <- predict(gam.m2, newdata = Wage) 


# GAM with local reg.
gam.lo <- gam(wage~s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.Gam(gam.lo, se = TRUE, col="green")


gam.lo.i <- gam(wage ~ lo(year, age, span=0.5) + education, data = Wage) 

library(akima)
plot(gam.lo.i)


# Logistic reg. GAM
gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")


table(Wage$education, I(Wage$wage > 250))


gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. < HS Grad"))
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")








## 7.9 Exercises --------------

## 6

library(boot)

cv.mse <- vector("double", length = 10)
models <- list()
for(i in 1:10){
  poly.fit <- glm(wage ~ poly(age, i), data = Wage)
  models[[i]] <- poly.fit
  cv.mse[[i]] <- cv.glm(Wage, poly.fit, K = 20)$delta [[2]]
}
plot(cv.mse)

anova(models[[1]], models[[2]], models[[3]], models[[4]], models[[5]], models[[6]], models[[7]], models[[8]], models[[9]], models[[10]])


poly.fit <- lm(wage ~ poly(age, 4) , Wage) ##wage ~ poly(age, 4) + I(age^9)
agelims <-  range(Wage$age)
age.grid <- seq(from=agelims[1], to = agelims[2])
pred <- predict(poly.fit, list(age = age.grid), se.fit = T)
plot(Wage$age, Wage$wage, col = "grey")
lines(age.grid, pred$fit, lwd = 2)
se.bands <- cbind(pred$fit +2* pred$se.fit ,pred$fit -2* pred$se.fit)
matlines (age.grid ,se.bands,lwd=1,col="blue",lty=3)



cv.mse <- vector("double", length = 9)
models <- list()
for(i in 2:10){
  Wage$cuts <- cut(Wage$age, i)
  cut.fit <- glm(wage ~ cuts, data = Wage)
  models[[i]] <- cut.fit
  cv.mse[[i-1]] <- cv.glm(Wage, cut.fit, K = 10)$delta[[2]]
}
plot(2:10, cv.mse)


cut.fit <- glm(wage ~ cut(age, 8), data = Wage)
agelims <-  range(Wage$age)
age.grid <- seq(from=agelims[1], to = agelims[2])
pred <- predict(cut.fit, list(age = age.grid), se.fit = T)
plot(Wage$age, Wage$wage, col = "grey")
lines(age.grid, pred$fit, lwd = 2)
se.bands <- cbind(pred$fit +2* pred$se.fit ,pred$fit -2* pred$se.fit)
matlines (age.grid ,se.bands,lwd=1,col="blue",lty=3)



## 7

?Wage

plot(Wage)

library(leaps)
bestsub <- regsubsets(wage~., Wage, nvmax = 11)
library(glmnet)
lasso <- cv.glmnet(x = model.matrix(wage~., Wage), y = Wage$wage, alpha = 1)

plot(lasso)
lasso$lambda.min
coef(lasso, s = lasso$lambda.min)

wage.fit <- gam(wage~jobclass + maritl + splines::ns(age, 6), data = Wage)

par(mfrow=c(1,3))
plot(wage.fit, se=TRUE, col="blue")


## 8
## -------------- meh


## 9

library(MASS)


plot(Boston$dis, Boston$nox)
poly.fit <- lm(nox ~ poly(dis, 3), data = Boston)

range.dis <- range(Boston$dis)
dis.grid <- seq(from = range.dis[1], to = range.dis[2], length.out = nrow(Boston))
preds <- predict(poly.fit, newdata = list(dis = dis.grid), se.fit = T)
lines(dis.grid, preds$fit, col = "red", lwd = 2)
matlines(dis.grid, preds$fit + cbind(2*preds$se.fit, -2*preds$se.fit), lty=3, col = "red")


range.dis <- range(Boston$dis)
dis.grid <- seq(from = range.dis[1], to = range.dis[2])

rss.error <- rep(0,10)
for(i in 1:10){
  poly.fit <- lm(nox ~ poly(dis, i), data = Boston)
  preds <- predict(poly.fit, newdata = list(dis = dis.grid), se.fit = T)
  rss.error[i] <- sum(poly.fit$residuals^2)
}
plot(rss.error)

library(boot)
cv.error <- rep(0,10)
for (i in 1:10) {
  glm.fit <- glm(nox~poly(dis, i), data = Boston)
  cv.error[i] <- cv.glm(Boston, glm.fit, K=10)$delta[1]  # [1]:std, [2]:bias-corrected
}
plot(cv.error)




fit.sp <- lm(nox~bs(dis, df = 4), data=Boston)
pred <- predict(fit.sp, newdata=list(dis=dis.grid), se=T)
plot(Boston$dis, Boston$nox, col="gray")
lines(dis.grid, pred$fit, lwd=2)
lines(dis.grid, pred$fit+2*pred$se, lty="dashed")
lines(dis.grid, pred$fit-2*pred$se, lty="dashed")

attr(bs(Boston$dis,df=4),"knots")



rss.error <- rep(0,7)
for (i in 4:10) {
  fit.sp <- lm(nox~bs(dis, df=i), data=Boston)
  rss.error[i-3] <- sum(fit.sp$residuals^2)
}
plot(4:10, rss.error, type="b")


et.seed(1)
cv.error <- rep(0,7)
for (i in 4:10) {
  glm.fit <- glm(nox~bs(dis, df=i), data=Boston)
  cv.error[i-3] <- cv.glm(Boston, glm.fit, K=10)$delta[1]
}
plot(4:10, cv.error, type="b")


## 10

train <- sample(1:nrow(College), nrow(College)/2)

forwardselection <- regsubsets(Outstate~., data = College[train, ], nvmax = 17, method = "forward")

predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

which.min(summary(forwardselection)$bic)

coef(forwardselection, id = 13)


err.fwd <- rep(NA, ncol(College)-1)
for(i in 1:(ncol(College)-1)) {
  pred.fwd <- predict(forwardselection, College[-train,], id=i)
  err.fwd[i] <- mean((College[-train,]$Outstate - pred.fwd)^2)
}
plot(err.fwd)
which.min(err.fwd)



library(gam)
gam.fit <- gam(Outstate ~ 
                 Private +   # categorical variable
                 s(Room.Board,5) + 
                 s(Terminal,5) + 
                 s(perc.alumni,5) + 
                 s(Expend,5) + 
                 s(Grad.Rate,5), 
               data=College)
par(mfrow=c(2,3))
plot(gam.fit, se=TRUE, col="blue")

pred <- predict(gam.fit, College[-train,])
(mse.error <- mean((College[-train,]$Outstate - pred)^2))
err.fwd[6]


summary(gam.fit)


## 11

X1 <- rnorm(100)
X2 <- rnorm(100)
eps <- rnorm(100)
Y <- -2.5 + 1.3 * X1 + 4.6 * X2 + eps
plot(Y)


b1_pred <- rnorm(1)


temp_y <- Y- b1_pred * X1
temp_lm <- lm(temp_y ~ X2)
b2_pred <- coef(temp_lm)[[2]]


temp_y <- Y- b2_pred * X2
temp_lm <- lm(temp_y ~ X1)
b1_pred <- coef(temp_lm)[[2]]

i <- 1
b0_pred <- b1_pred <- b2_pred <- rep(0, 1000)
while(i <= 1000){
  
  temp_y <- Y- b1_pred[[i]] * X1
  temp_lm <- lm(temp_y ~ X2)
  b2_pred[[i+1]] <- coef(temp_lm)[[2]]
  
  
  temp_y <- Y- b2_pred[[i]] * X2
  temp_lm <- lm(temp_y ~ X1)
  b1_pred[[i+1]] <- coef(temp_lm)[[2]]
  
  temp_y <- Y- b2_pred[[i]] * X2 - b1_pred[[i]] * X1
  temp_lm <- lm(temp_y ~ 1)
  b0_pred[[i+1]] <- coef(temp_lm)[[1]]
  
  i = i + 1
}


plot(b0_pred[1:30], type = "l")
plot(b1_pred[1:30], type = "l")
plot(b2_pred[1:30], type = "l")


plot(b0_pred[1:30], type = "l", ylim = c(-3, 6), col = "green")
lines(b1_pred[1:30], type = "l",  col = "red")
lines(b2_pred[1:30], type = "l",  col = "blue")

abline(-2.5, 0)
abline(1.3, 0)
abline(4.6, 0)



## 12

p = 100
n = 1000
betas <- rnorm(p+1)*5 
eps <- rnorm(n)
Y <- betas[1] + (X %*% betas[-1]) + eps
plot(Y)


fit.lm <- lm(Y~X)
bhats.lm <- coef(fit.lm)

bhats <- matrix(0, ncol=p, nrow=100)
mse.error <- rep(0, 100)
for (i in 1:100) {
  for (k in 1:p) {
    a = Y - (X[,-k] %*% bhats[i,-k])
    bhats[i:100,k] = lm(a ~ X[,k])$coef[2]
  }
  mse.error[i] <- mean((Y - (X %*% bhats[i,]))^2)
}
plot(mse.error[1:15])
