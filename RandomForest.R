# Link:
# https://www.r-bloggers.com/2018/01/how-to-implement-random-forests-in-r/


library(randomForest)
library(datasets)
library(caret)
library(lattice)

data <- read.csv("Data_blanks.csv")
str(data)

data$Unit <- as.factor(data$Unit)

table(data$Unit)

set.seed(666) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]


mtry <- sqrt(ncol(data)) #to set the mtry as suggested in the package

rf <- randomForest(Unit~., data=train, ntree = 1500, mtry = mtry, proximity=TRUE) 
print(rf)

rf_2 <- randomForest(Unit~., data=train, ntree = 1500, mtry = 1, proximity=TRUE) 
print(rf_2)

#NB:
# The first parameter specifies our formula: Species ~ . (we want to predict Species using each of the remaining columns of data).
# –ntree defines the number of trees to be generated. It is typical to test a range of values for this parameter (i.e. 100,200,300,400,500) and choose the one that minimises the OOB estimate of error rate.
# –mtry is the number of features used in the construction of each tree. These features are selected at random, which is where the “random” in “random forests” comes from. The default value for this parameter, when performing classification, is sqrt(number of features).
# –importance enables the algorithm to calculate variable importance.



p1 <- predict(rf, train)
confusionMatrix(p1, train$Unit)


p2 <- predict(rf, test)
confusionMatrix(p2, test$Unit)


plot(rf)

plot(rf_2)

p2_2 <- predict(rf_2, test)
confusionMatrix(p2_2, test$Unit)

hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf)


varImpPlot(rf_2,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf_2)



partialPlot(rf, train, PC1, "gic")


MDSplot(rf, train$Unit)

MDSplot(rf_2, train$Unit)

# Using For loop to identify the right mtry for model
a=c()
i=11
for (i in 3:11) {
  model_with_loop <- randomForest(Unit~., data=train, ntree = 1500, mtry = i, importance = TRUE)
  predValid <- predict(model_with_loop, test, type = "class")
  a[i-2] = mean(predValid == test$Blank)
}

a

plot(3:11,a)


# Random Search of mtry (https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(666)
#mtry <- sqrt(ncol(x)) #already have it
metric <- "Accuracy" #not clear why he does that, maybe ust for teaching
rf_random <- train(Unit~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control) #still not clear the tuneLength
print(rf_random)
plot(rf_random)


# Algorithm Tune (tuneRF) in the same link as above, quite interesting
set.seed(666)
bestmtry <- tuneRF(train[,-1], train[,1], stepFactor=1.5, improve=1e-5, ntree=2000) #improve is maybe not so much needed
print(bestmtry)



# Manual Search of ntree ----------
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train)))) #or should I put simply the whole data (i.e, data)?
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(666)
  fit <- train(Blank~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)


#Other info and more ways: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
