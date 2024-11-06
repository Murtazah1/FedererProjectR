# knn section


library(e1071)
library(ISLR)
library(class)

dataset <- federerR.dataset.final

dataset$Date = NULL
dataset$ATP = NULL



dataset$Location = as.integer(as.factor(dataset$Location))

dataset$Tournament = as.integer(as.factor(dataset$Tournament))

dataset$Series = as.integer(as.factor(dataset$Series))

dataset$Court = as.integer(as.factor(dataset$Court))

dataset$Surface = as.integer((as.factor(dataset$Surface)))

dataset$Round = as.integer(as.factor(dataset$Round))

dataset$Best.of = as.integer((as.factor(dataset$Best.of)))

dataset$Contestant = as.integer((as.factor(dataset$Contestant)))

dataset$WRank = as.integer(as.factor(dataset$WRank))

dataset$LRank = as.integer(as.factor(dataset$LRank))

dataset$Wsets = as.integer(as.factor(dataset$Wsets))

dataset$Lsets = as.integer(as.factor(dataset$Lsets))

dataset$Comment = as.integer(as.factor(dataset$Comment))

dataset$Outcome = as.integer(as.factor(dataset$Outcome))


n <- nrow(dataset)

RNGkind(sample.kind = "Rounding")
set.seed(1)


dataset <- na.omit(dataset)

trainer <- sample(1:n, 406)

train_set <- dataset[trainer,]
test_set <- dataset[-trainer,]







trainSet <- na.omit(train_set)
testSet <- na.omit(test_set)






for (K in c(1,3,5,10,15,20,25,30)){
  set.seed(1)
  knn.pred <- knn(train = trainSet[-14],
                  test = testSet[-14],
                  cl = trainSet$Outcome,
                  k = K)
  print(mean(knn.pred != testSet$Outcome))
  print(table(Guess = knn.pred, Correct = testSet$Outcome))
  
}


for (K in c(1,3,5,10,15,20, 25, 30)){
  set.seed(1)
  knn.pred.cross <- knn.cv(train = dataset[-14],
                  cl = dataset$Outcome,
                  k = K)
  print(mean(knn.pred.cross != dataset$Outcome))
  print(table(Guess = knn.pred.cross,Correct = dataset$Outcome))
  
}




# start of svm section




library(e1071)
library(ISLR)
library(class)

dataset <- federerR.dataset.final

dataset$Date = NULL
dataset$ATP = NULL

dataset$Location = as.integer(as.factor(dataset$Location))

dataset$Tournament = as.integer(as.factor(dataset$Tournament))

dataset$Series = as.integer(as.factor(dataset$Series))

dataset$Court = as.integer(as.factor(dataset$Court))

dataset$Surface = as.integer((as.factor(dataset$Surface)))

dataset$Round = as.integer(as.factor(dataset$Round))

dataset$Best.of = as.integer((as.factor(dataset$Best.of)))

dataset$Contestant = as.integer((as.factor(dataset$Contestant)))

dataset$WRank = as.integer(as.factor(dataset$WRank))

dataset$LRank = as.integer(as.factor(dataset$LRank))

dataset$Wsets = as.integer(as.factor(dataset$Wsets))

dataset$Lsets = as.integer(as.factor(dataset$Lsets))

dataset$Comment = as.integer(as.factor(dataset$Comment))

dataset$Outcome = as.integer(as.factor(dataset$Outcome))


n <- nrow(dataset)

RNGkind(sample.kind = "Rounding")
set.seed(1)

trainer <- sample(1:n, 409)

train_set <- dataset[trainer,]
test_set <- dataset[-trainer,]



trainSet <- na.omit(train_set)
testSet <- na.omit(test_set)

trainFrame <- data.frame(trainSet[,-14], Outcome = as.factor(trainSet$Outcome))
testFrame <- data.frame(testSet[,-14], Outcome = as.factor(testSet$Outcome))

set.seed(1)

fit <- svm(Outcome ~ ., data = trainFrame, cost = 1, kernel = 'linear')


pred <- predict(fit, testFrame)


set.seed(1)
tune.out.linear=tune(svm,
                     Outcome~.,
                     data=trainFrame,
                     kernel="linear",
                     ranges=list(cost=c(0.01,0.5,1,5,10)))

summary(tune.out.linear)


set.seed(1)
tune.out.poly=tune(svm,
                   Outcome~.,
                   data=trainFrame,
                   kernel="polynomial",
                   ranges=list(cost=c(0.01,0.5,1,5,10)))

summary(tune.out.poly)


set.seed(1)
tune.out.radial=tune(svm,
                     Outcome~.,
                     data=trainFrame,
                     kernel="radial",
                     ranges=list(cost=c(0.01,0.5,1,5,10)))

summary(tune.out.radial)

ypred <- predict(tune.out.linear$best.model,testFrame)
table(predict=ypred, truth=testFrame$Outcome)

ypred <- predict(tune.out.poly$best.model,testFrame)
table(predict=ypred, truth=testFrame$Outcome)

ypred <- predict(tune.out.radial$best.model,testFrame)
table(predict=ypred, truth=testFrame$Outcome)







