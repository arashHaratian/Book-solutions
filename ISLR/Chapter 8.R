## 8.3 Lab: Decision Trees ----------------------------------
## 8.3.1 Fitting Classiﬁcation Trees -----------------------------

library(ISLR)
library(tree)

High <- ifelse(Carseats$Sales > 8, "Yes", "No")

Carseats <- data.frame(Carseats, High)

tree.carseats <- tree(High~.-Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)


train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High~.-Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)

cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
plot(cv.carseats$size, cv.carseats$dev)
plot(cv.carseats$k, cv.carseats$dev)


prune.carseats <- prune.misclass(tree.carseats, best = 13)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)


## 8.3.2 Fitting Regression Trees -----------------------

library(MASS)

train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston, subset = train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 0)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev)

prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline (0,1)
mean((yhat - boston.test)^2) 


## 8.3.3 Bagging and Random Forests ----------------------

library(randomForest)

bag.boston <- randomForest(medv~., data = Boston, subset = train, mtry=13, importance = TRUE)
bag.boston

yhat.bag <- predict(bag.boston, newdata = Boston[-train,])

plot(yhat.bag, boston.test)
abline (0,1)
mean((yhat.bag - boston.test)^2)

bag.boston <- randomForest(medv~., data = Boston, subset = train, mtry=13, ntree = 25)

yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2)

rf.boston <- randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2)

importance(rf.boston)
varImpPlot (rf.boston)


## 8.3.4 Boosting -------------------

library(gbm)

boost.boston <- gbm(medv~., data = Boston[train,], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

yhat.boost <- predict(boost.boston, newdata = Boston[-train ,], n.trees = 5000)
mean((yhat.boost - boston.test)^2) 


boost.boston <- gbm(medv~., data = Boston[train,], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.02, verbose = F)
yhat.boost=predict (boost.boston, newdata = Boston[-train ,], n.trees=5000)
mean((yhat.boost - boston.test)^2)








## 8.4 Exercises ------------------------------------

## 7

train=sample(1:nrow(Boston),nrow(Boston)/2)
ntree.err <- vector("double", 500)
for(i in 1:500){
boston.rf <- randomForest(medv~., data = Boston, subset = train, ntree = i)
pred <- predict(boston.rf, Boston[-train, ])
ntree.err[[i]] <- mean((pred - Boston[-train, 13])^2)
}
plot(ntree.err,type = "l")


mtry.err <- vector("double", 500)
for(i in 1:13){
  boston.rf <- randomForest(medv~., data = Boston, subset = train, mtry = i)
  pred <- predict(boston.rf, Boston[-train, ])
  mtry.err[[i]] <- mean((pred - Boston[-train, 13])^2)
}
plot(mtry.err, type = "l")


## 8


train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]

tree.carseats <- tree(Sales~., Carseats, subset = train)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
tree.pred <- predict(tree.carseats, Carseats.test)
mean((tree.pred - Carseats.test$Sales)^2)


cv.carseats <- cv.tree(tree.carseats)
names(cv.carseats)
plot(cv.carseats$size, cv.carseats$dev)
plot(cv.carseats$k, cv.carseats$dev)


prune.carseats <- prune.tree(tree.carseats, best = 7)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats.test)
mean((tree.pred - Carseats.test$Sales)^2)



car.bag <- randomForest(Sales~., data = Carseats, subset = train, ntry = 10)
pred <- predict(car.bag, Carseats.test)
mean((pred - Carseats.test$Sales)^2)

importance(car.bag)
varImpPlot(car.bag)


car.rf <- randomForest(Sales~., data = Carseats, subset = train, ntry= 3)
pred <- predict(car.rf, Carseats.test)
mean((pred - Carseats.test$Sales)^2)

importance(car.rf)
varImpPlot(car.rf)


## 9

?OJ
dim(OJ)

train <- sample(1:nrow(OJ), 800)
OJ.test <- OJ[-train,]

OJ.tree <- tree(Purchase~., OJ, subset = train)
plot(OJ.tree)
text(OJ.tree, pretty = 0)
summary(OJ.tree)

pred <- predict(OJ.tree, OJ.test, type = "class")
table(pred, OJ.test$Purchase)


OJ.cv <- cv.tree(OJ.tree, FUN = prune.misclass)
plot(OJ.cv$size, OJ.cv$dev)
plot(OJ.cv$k, OJ.cv$dev)

prune.OJ <- prune.misclass(OJ.tree, best = 4)
summary(prune.OJ)

pred <- predict(prune.OJ, OJ.test, type = "class")
table(pred, OJ.test$Purchase)


## 11

Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)


train <- sample(1:nrow(Hitters), 200)
Hitters.train <- Hitters[train,]
Hitters.test <- Hitters[-train, ]
library(gbm)

train.err <- vector("double", 100)
shrinkage.seq <- seq(from = 0.001, to = 1, length.out = 100)
for(i in 1:length(shrinkage.seq)){
Hitters.gbm <- gbm(Salary~., data = Hitters.train, n.trees = 1000, shrinkage = shrinkage.seq[[i]], distribution = "gaussian")
pred <- predict(Hitters.gbm)
train.err[i] <- mean((pred - Hitters.train$Salary)^2)
}
plot(shrinkage.seq, train.err)



test.err <- vector("double", 100)
shrinkage.seq <- seq(from = 0.001, to = 1, length.out = 100)
for(i in 1:length(shrinkage.seq)){
  Hitters.gbm <- gbm(Salary~., data = Hitters.train, n.trees = 1000, shrinkage = shrinkage.seq[[i]], distribution = "gaussian")
  pred <- predict(Hitters.gbm, newdata = Hitters.test)
  test.err[i] <- mean((pred - Hitters.test$Salary)^2)
}
plot(shrinkage.seq, test.err)


## 11
library(ISLR)
library(gbm)
Caravan$Purchase <- ifelse(Caravan$Purchase == "Yes",1,0)
Caravan.train <- Caravan[1:1000,]
Caravan.test <- Caravan[-c(1:1000),]

Caravan.gbm <- gbm(Purchase~.,data=Caravan.train,n.trees = 1000,shrinkage = 0.01,distribution = "bernoulli")
summary(Caravan.gbm)
pred <- predict(Caravan.gbm, newdata = Caravan.test, type = "response")
Caravan.pred <- ifelse(pred > 0.2, 1, 0)
table(Caravan.test$Purchase,Caravan.pred)


Caravan.glm <- glm(Purchase~., family='binomial', data = Caravan.train)
Caravan.pred <- predict(Caravan.glm, Caravan.test, type='response')
Caravan.pred <- ifelse(Caravan.pred > 0.2, 1, 0)
table(Caravan.test$Purchase, Caravan.pred)
