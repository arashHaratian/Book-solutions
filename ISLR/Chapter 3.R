## ###########################Linear Regression ##############################
library(MASS)
library(ISLR)


## 3.6.2 Simple Linear Regression --------------------------
?Boston
fix(Boston)
names(Boston)


lm.fit <- lm(medv ~ lstat, Boston)

lm.fit
summary(lm.fit)

names(lm.fit)

coef(lm.fit)

confint(lm.fit)


predict(lm.fit ,data.frame(lstat=c(5,10,15)), interval="confidence")


predict(lm.fit ,data.frame(lstat=c(5,10,15)), interval="prediction")


plot(Boston$lstat ,Boston$medv)
abline(lm.fit)
##--
ggplot(Boston) +
  geom_point(aes(lstat, medv)) +
  geom_abline(intercept = coef(lm.fit)[1], slope = coef(lm.fit)[2])



abline(lm.fit ,lwd=3)
abline(lm.fit ,lwd=3,col="red")
plot(Boston$lstat ,Boston$medv ,col="red")
plot(Boston$lstat ,Boston$medv ,pch=20)
plot(Boston$lstat ,Boston$medv ,pch="+")
plot(1:20,1:20,pch =1:20) 


plot(lm.fit)

par(mfrow = c(1,1))
plot(lm.fit) 



plot(predict (lm.fit), residuals (lm.fit))
plot(predict (lm.fit), rstudent (lm.fit))


plot(hatvalues(lm.fit))
which.max(hatvalues (lm.fit))



## 3.6.3 Multiple Linear Regression -----------------------
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
?summary.lm
summary(lm.fit)$r.sq
summary(lm.fit)$sigma


library (car)

vif(lm.fit)


lm.fit1 <- lm(medv~.-age, data = Boston)
summary(lm.fit1)
vif(lm.fit1)
lm.fit1 <- update(lm.fit, ~.-age)
lm.fit1 <- update(lm.fit1, ~.-indus)




## 3.6.4 Interaction Terms----------------------
summary(lm(medv ~ lstat*age, data = Boston))


## 3.6.5 Non-linear Transformations of the Predictors ----------------


lm.fit2=lm(medv~lstat+I(lstat^2), data = Boston)
summary(lm.fit2)


lm.fit <- lm(medv~lstat , Boston)
anova(lm.fit ,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5 <- lm(medv~poly(lstat,5), Boston)
summary (lm.fit5)
par(mfrow=c(2,2))
plot(lm.fit5)

summary (lm(medv~log(rm),data=Boston))

## 3.6.6 Qualitative Predictors ------------------

fix(Carseats)
names(Carseats)


lm.fit <- lm(Sales ~ .+ Income:Advertising + Price:Age, data = Carseats) 
summary(lm.fit)

?contrasts
contrasts(Carseats$ShelveLoc)


## 3.7 Exercises ----------------------------
## 8
?Auto

par(mfrow = c(1,1))
plot(mpg~horsepower, Auto)
lm.fit <- lm(mpg~horsepower, Auto)
summary(lm.fit)
abline(lm.fit)

predict(lm.fit, data.frame(horsepower = (98)))
predict(lm.fit, data.frame(horsepower = (98)), interval = "confidence")
predict(lm.fit, data.frame(horsepower = (98)), interval = "prediction")

plot(lm.fit)

## 9


plot(Auto)

cor(Auto[-9])

lm.fit2 <- lm(mpg ~ . -name, Auto)
summary(lm.fit2)


plot(lm.fit2)


lm.fit2 <- lm(mpg ~ . -name + horsepower*weight, Auto)
summary(lm.fit2)
plot(lm.fit2)



lm.fit2 <- lm(mpg ~  poly(horsepower,2)  + year + origin + poly(horsepower,2)*weight, Auto)
summary(lm.fit2)
plot(lm.fit2)


## 10

?Carseats 
lm.fit1 <- lm(Sales ~ Urban + US + Price, Carseats)
summary(lm.fit1)
plot(lm.fit1)

lm.fit2 <- lm(Sales ~ US + Price, Carseats)
summary(lm.fit2)
plot(lm.fit2)

anova(lm.fit1, lm.fit2)


confint(lm.fit2)


which.max(hatvalues(lm.fit2))


## 11

set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)

lm.fit1 <- lm(y ~ x -1) # y~x+0
summary(lm.fit1)

plot(x,y)
abline(lm.fit1)

lm.fit2 <- lm(x ~ y + 0)
summary(lm.fit2)
plot(y,x)
abline(lm.fit1)


summary(lm.fit1)
summary(lm.fit2)



## 12




## 13

set.seed(1)

x <- rnorm(100, 0, 1)
eps <- rnorm(100, 0, 0.25)
y <- -1 + 0.5 * x + eps ## Beta0 = -1 , Beta1 = 0.5
length(y)


plot(x, y)

lm.fit <- lm(y ~ x)
coef(lm.fit)
abline(lm.fit)
abline(-1,0.5)
summary(lm.fit)

lm.fit2 <- lm(y ~ x + I(x^2))
coef(lm.fit2)
abline(lm.fit2)
summary(lm.fit2)

anova(lm.fit, lm.fit2)



x2 <- rnorm(100, 0, 1)
eps2 <- rnorm(100, 0, 0.7)
y2 <- -1 + 0.5 * x2 + eps2 ## Beta0 = -1 , Beta1 = 0.5
length(y2)


plot(x2, y2)

lm.fit2 <- lm(y2 ~ x2)
coef(lm.fit2)
abline(lm.fit2)
abline(-1,0.5)
summary(lm.fit2)

lm.fit3 <- lm(y2 ~ x2 + I(x2^2))
coef(lm.fit3)
abline(lm.fit3)
summary(lm.fit3)

anova(lm.fit2, lm.fit3)



x <- rnorm(100, 0, 1)
eps <- rnorm(100, 0, 0.05)
y <- -1 + 0.5 * x + eps ## Beta0 = -1 , Beta1 = 0.5
length(y)


plot(x, y)

lm.fit <- lm(y ~ x)
coef(lm.fit)
abline(lm.fit)
abline(-1,0.5)
summary(lm.fit)

lm.fit2 <- lm(y ~ x + I(x^2))
coef(lm.fit2)
abline(lm.fit2)
summary(lm.fit2)

anova(lm.fit, lm.fit2)





## 14

set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100) ##  2, 2, 0.3


cor(x1, x2)
plot(x1, x2)


lm.fit <- lm(y ~ x1+x2)
summary(lm.fit)


lm.fit1 <- lm(y ~ x1)
summary(lm.fit1)


lm.fit2 <- lm(y ~ x2)
summary(lm.fit2)


x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y,6)




lm.fit <- lm(y ~ x1+x2)
summary(lm.fit)


lm.fit1 <- lm(y ~ x1)
summary(lm.fit1)


lm.fit2 <- lm(y ~ x2)
summary(lm.fit2)


plot(x1, x2)
plot(lm.fit)



## 15

?Boston

names(Boston)
lm.fit <- lapply(names(Boston)[-1], FUN = function(x) lm(formula = paste("crim ~", x), Boston))

coefs <- sapply(lm.fit, coef)


lm.fit2 <- lm(crim~., Boston)
summary(lm.fit2)

plot(coefs[2,], coef(lm.fit2)[-1])

plot(Boston)
