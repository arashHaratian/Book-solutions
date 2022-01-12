## 5.3.1 The Validation Set Approach ------------------------------

library(ISLR)
set.seed(1)
train <- sample(392, 196)

lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)

mean((Auto$mpg-predict(lm.fit, Auto))[-train]^2)


lm.fit2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit2, Auto))[-train]^2)


lm.fit3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit2, Auto))[-train]^2)


##--

set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit2, Auto))[-train]^2)



## 5.3.2 Leave-One-Out Cross-Validation ----------------------------------

glm.fit <- glm(mpg~horsepower, data = Auto)
coef(glm.fit)
lm.fit <- lm(mpg~horsepower, data = Auto)
coef(lm.fit)


library(boot)
glm.fit <- glm(mpg~horsepower, data = Auto)
cv.err <- cv.glm(data = Auto, glmfit = glm.fit)
cv.err$delta


cv.error <- rep(0, 5)
for (i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error


## 5.3.3 k-Fold Cross-Validation --------------


set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error


## 5.3.4 The Bootstrap ---------------------------


?Portfolio
alpha.fn <- function(data ,index){
  X <- data$X[index]
  Y <- data$Y[index]
  return ((var(Y)-cov(X,Y))/(var(X)+var(Y) -2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)


set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T)) ## use loops to do bootstrapping

boot(Portfolio, alpha.fn, R = 1000)




boot.fn <- function (data, index)
  return(coef(lm(mpg~horsepower, data = data, subset = index)))

boot.fn(Auto, 1:392)

boot.fn(Auto, sample(392, 392, replace=T))
boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower, data = Auto))$coef 


boot.fn <- function (data, index)
  coefficients(lm(mpg~horsepower+I(horsepower^2), data = data, subset = index))
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef


## 5.4 Exercises ------------------------------------------


## 2

plot(1:10000, 1-(1-1/1:10000)^(1:10000))


store <- rep(NA, 10000)
for(i in 1:10000) {
  store[i] <- sum(sample (1:100, rep=TRUE)==4) >0
}
mean(store)


## 5

glm.fit <- glm(default ~ income + balance, data = Default, family = "binomial")



index <- sample(10000, 5000)
Default.train <- Default[index,]
Default.test <- Default[-index,]

glm.fit <- glm(default ~ income + balance, data = Default.train, family = "binomial")
glm.prob <- predict(glm.fit, Default.test, type = "response")
glm.pred <- ifelse(glm.prob>0.5, "Yes", "No")
mean(glm.pred != Default.test$default)

glm.fn <- function(data, index){
  glm.fit <- glm(default ~ income + balance, data = data, subset = index, family = "binomial")
  glm.prob <- predict(glm.fit, Default.test, type = "response")
  glm.pred <- ifelse(glm.prob>0.5, "Yes", "No")
  mean(glm.pred != Default.test$default)
}
boot(Default.train, glm.fn, 1000)




glm.fit <- glm(default ~ income + balance + student, data = Default.train, family = "binomial")
glm.prob <- predict(glm.fit, Default.test, type = "response")
glm.pred <- ifelse(glm.prob>0.5, "Yes", "No")
mean(glm.pred != Default.test$default)

glm.fn <- function(data, index){
  glm.fit <- glm(default ~ income + balance + student, data = data, subset = index, family = "binomial")
  glm.prob <- predict(glm.fit, Default.test, type = "response")
  glm.pred <- ifelse(glm.prob>0.5, "Yes", "No")
  mean(glm.pred != Default.test$default)
}
boot(Default.train, glm.fn, 1000)

## 6

glm.fit <- glm(default ~ income + balance, data=Default, family=binomial)
summary(glm.fit)


glm.fn <- function(data, index){
  glm.fit <- glm(default ~ income + balance, data = data, subset = index, family = "binomial")
  coef(glm.fit)
}
boot(Default, glm.fn, 1000)



## 7

glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)

glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1,], family = binomial)
ifelse(predict(glm.fit, Weekly[1,], type = "response"), "Up", "Down") == Weekly$Direction[1]


result <- vector(length = nrow(Weekly))
for (i in 1:nrow(Weekly)){
  glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i,], family = binomial)
  result[i] <- ifelse(predict(glm.fit, Weekly[i,], type = "response"), "Up", "Down") != Weekly$Direction[i]
}

mean(result)


## 8
set.seed(1)
x <- rnorm(100) 
y <- x - 2 * x ^ 2 + rnorm(100)

plot(x, y)


set.seed(1)
glm.fit <- glm(y ~ x)
cv.err <- cv.glm(sim, glm.fit)
cv.err$delta
summary(glm.fit)

glm.fit <- glm(y ~ poly(x, 2))
cv.err <- cv.glm(sim, glm.fit)
cv.err$delta
summary(glm.fit)

glm.fit <- glm(y ~ poly(x, 3))
cv.err <- cv.glm(sim, glm.fit)
cv.err$delta
summary(glm.fit)

glm.fit <- glm(y ~ poly(x, 4))
cv.err <- cv.glm(sim, glm.fit)
cv.err$delta
summary(glm.fit)



## 9

library(MASS)

mu <- mean(Boston$medv)
mu.sd <- sd(Boston$medv) / sqrt(nrow(Boston))
mu.boot <- boot(Boston, function(data, index) {mean(data[index, ]$medv)}, 1000)
mu.ci <- c(mu - 2*sd(mu.boot$t), mu + 2*sd(mu.boot$t))

t.test(Boston$medv)
mu.ci


med <- median(Boston$medv)
med.boot <- boot(Boston, function(data, index) {median(data[index, ]$medv)}, 1000)
med.ci <- c(mu - 2*sd(med.boot$t), mu + 2*sd(med.boot$t))

t.test(Boston$medv)
mu.ci

medv.mu10 <- quantile(Boston$medv, 0.1)
medv.mu10.boot <- boot(Boston, function(data, index) {quantile(data[index, ]$medv, 0.1)}, 1000)
medv.mu10
medv.mu10.boot
