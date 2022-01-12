library(ISLR)



names(Smarket)
?Smarket

summary(Smarket)

cor(Smarket[,-9])

plot(Smarket$Volume)

## 4.6.2 Logistic Regression  -------------------------------

?glm
glm.fits <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family = binomial) 
summary(glm.fits)

coef(glm.fits)

summary(glm.fits)$coef

summary (glm.fits)$coef[,4]


glm.pred <- predict(glm.fits, type = "response")
glm.pred[1:10]
contrasts(Smarket$Direction)


glm.pred <- ifelse(glm.pred > 0.5, "Up", "Down")

table(glm.pred, Smarket$Direction)

mean(glm.pred == Smarket$Direction)


train <- Smarket$Year < 2005
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!train]

glm.fits <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = "binomial", subset = train)

glm.probs <- predict(glm.fits, Smarket.2005, type = "response")

glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred!=Direction.2005)




glm.fits <- glm(Direction ~ Lag1+Lag2, data = Smarket, family = "binomial", subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)



predict(glm.fits, newdata = data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.1,-0.8)), type = "response")


## 4.6.3 Linear Discriminant Analysis -----------------------------


library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset = train)
lda.fit

# plot(-0.6420190*Smarket$Lag1 -0.5135293*Smarket$Lag1, Smarket$Direction)

lda.pred <- predict(lda.fit, Smarket.2005)
lda.pred


lda.class <- lda.pred$class 
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005) 

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)

lda.pred$posterior[1:20,1]
lda.class [1:20]

sum(lda.pred$posterior[,1]>.9)


## 4.6.4 Quadratic Discriminant Analysis ---------------------

library(MASS)
?qda

qda.fit <- qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda.fit

qda.class <- predict(qda.fit, Smarket.2005)$class
qda.class

table(qda.class, Direction.2005)

mean(qda.class==Direction.2005) 


## 4.6.5 K-Nearest Neighbors ------------------------------

library(class)

train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train ,]
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train ,]
train.Direction <- Smarket$Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1) 
table(knn.pred, Direction.2005) 
(83+43) /252 

knn.pred <- knn(train.X, test.X, train.Direction, k = 3) 
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) 


## 4.6.6 An Application to Caravan Insurance Data -------------------

?Caravan

summary (Caravan$Purchase) 

standardized.X <- scale(Caravan[,-86])

var(Caravan [,1])
var(Caravan [,2])
var(standardized.X[,1])
var(standardized.X[,2])


test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Caravan$Purchase [-test]
test.Y <- Caravan$Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y!=knn.pred) 


knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)

knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)

glm.fits <- glm(Purchase~., data=Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fits, Caravan[test,], type = "response")
glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")
table(glm.pred, test.Y)

glm.pred <- ifelse(glm.probs > 0.25, "Yes", "No")
table(glm.pred, test.Y)



## 4.7 Exercises ###################################

## 10

?Weekly
dim(Weekly)

summary(Weekly)
cor(Weekly[,-9])
summary(Weekly$Direction)

plot(Weekly)

glm.fit <- glm(Direction ~.-Today - Year, Weekly, family = "binomial")
glm.fit
summary(glm.fit)
glm.pred <- ifelse(predict(glm.fit, type ='response') > 0.5, "Up", "Down")
table(glm.pred, Weekly$Direction)


index <- Weekly$Year < 2009

train <- Weekly[index,]
test <- Weekly[!index,]

glm.fit <- glm(Direction ~ Lag2, Weekly, subset = index, family = "binomial")
summary(glm.fit)
glm.pred <- ifelse(predict(glm.fit, test[,-9], type ='response') > 0.5, "Up", "Down")
table(glm.pred, test$Direction)
mean(glm.pred == test$Direction)


library(MASS)
lda.fit <- lda(Direction ~ Lag2, Weekly, subset = index)
summary(lda.fit)
lda.pred <- predict(lda.fit, test[,-9])$class
table(lda.pred, test$Direction)
mean(lda.pred == test$Direction)


qda.fit <- qda(Direction ~ Lag2, Weekly, subset = index)
summary(qda.fit)
qda.pred <- predict(qda.fit, test[,-9])$class
table(qda.pred, test$Direction)
mean(qda.pred == test$Direction)


library(class)
knn.pred <- knn(as.matrix(train[,3]), as.matrix(test[,3]), train[,9], k = 1)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)


#--

glm.fit <- glm(Direction ~.-Today - Year, Weekly, subset = index, family = "binomial")
summary(glm.fit)
glm.pred <- ifelse(predict(glm.fit, test[,-9], type ='response') > 0.5, "Up", "Down")
table(glm.pred, test$Direction)
mean(glm.pred == test$Direction)


library(MASS)
lda.fit <- lda(Direction ~.-Today - Year, Weekly, subset = index)
summary(lda.fit)
lda.pred <- predict(lda.fit, test[,-9])$class
table(lda.pred, test$Direction)
mean(lda.pred == test$Direction)


qda.fit <- qda(Direction ~.-Today - Year, Weekly, subset = index)
summary(qda.fit)
qda.pred <- predict(qda.fit, test[,-9])$class
table(qda.pred, test$Direction)
mean(qda.pred == test$Direction)


library(class)
knn.pred <- knn(train[,2:6], test[,2:6], train[,9], k = 1)
table(knn.pred, test$Direction)
mean(knn.pred == test$Direction)


## 11

?Auto
dim(Auto)

cars <- Auto
cars$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
plot(cars)


train = (cars$year%%2 == 0)  # if the year is even
Auto.train = cars[train, ]
Auto.test = cars[!train, ]
cars.test = cars[!train,]


lda.fit <- lda(mpg01~cylinders+displacement+horsepower+acceleration, cars, subset = train)
lda.pred<- predict(lda.fit, Auto.test)
table(lda.pred, Auto.test$mpg01)
mean(lda.pred$class == cars.test$mpg01)


qda.fit <- qda(mpg01~cylinders+displacement+horsepower+acceleration, cars, subset = train)
qda.pred<- predict(qda.fit, Auto.test)
table(qda.pred, Auto.test$mpg01)
mean(qda.pred$class == cars.test$mpg01)


glm.fit <- glm(mpg01~cylinders+displacement+horsepower+acceleration, cars, subset = train, family = "binomial")

glm.pred <- predict(glm.fit, Auto.test, type = "response")
glm.pred <- ifelse(glm.pred>0.5, 1, 0)
table(glm.pred, Auto.test$mpg01)
mean(glm.pred == cars.test$mpg01)


knn.pred <- knn(Auto.train[,c(2,3,4,6)], Auto.test[,c(2,3,4,6)], Auto.train[,10], k = 1)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred == cars.test$mpg01)

knn.pred <- knn(Auto.train[,c(2,3,4,6)], Auto.test[,c(2,3,4,6)], Auto.train[,10], k = 10)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred == cars.test$mpg01)
knn.pred <- knn(Auto.train[,c(2,3,4,6)], Auto.test[,c(2,3,4,6)], Auto.train[,10], k = 100)
table(knn.pred, Auto.test$mpg01)
mean(knn.pred == cars.test$mpg01)


## 12

power <- function(){
  2^3
}
power()

power2 <- function(x, a){
  x^a
}
power2(3,8.2)
power2(10,3)
power2(8,17)
power2(131,3)


x <- 1:10
plot(x, power2(x, 2), ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")
plot(x, power2(x, 2), log = "xy", ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")
plot(x, power2(x, 2), log = "y", ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")


PlotPower <- function(x, a) {
  plot(x, power2(x, a))
}
PlotPower(1:10, 3)


## 13

library(MASS)
summary(Boston)


data <- Boston
data$crim01 <- ifelse(Boston$crim > median(Boston$crim), 1, 0)

cor(data)
plot(data)

train <-  1:(dim(data)[1]/2)
test <- (dim(data)[1]/2 + 1):dim(data)[1]
data.train <- data[train, ]
data.test <- data[test, ]

glm.fit <-  glm(crim01 ~ .- crim, data = data, family = "binomial", subset = train)
glm.probs <- predict(glm.fit, data.test, type = "response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
mean(glm.pred != data.test$crim01)


lda.fit <-  lda(crim01 ~ .- crim, data = data, subset = train)
lda.probs <- predict(lda.fit, data.test)$class
mean(lda.probs != data.test$crim01)



