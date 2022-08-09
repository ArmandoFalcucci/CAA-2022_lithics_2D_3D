library(rgl) #v.0.106.8
library(shapes) #v.1.2.6
library(tripack) #v1.3.9.1
library(MASS) #v.7.3.54
library(lmtest) #v.0.9.38
library(ape) #v.5.5
library(ade4) #v.1.7.16
library(pls) #v.2.7.3
library(Morpho) #v.2.8
library(geomorph) #v.4.0.0
library(geometry) #v.0.4.5
library(car) #v.3.0.10
library(grDevices) #v.4.1.0
library(factoextra) #v.1.0.7
library(vegan) #v.2.5.7
library(randomForest) #v.4.6.14
library(mda) #v.0.5.2
library(RColorBrewer) #v.1.1.2
library(caret)#v.6.0.88
library(earth)#v.5.3.0
library(RVAideMemoire)#v.0.9.79




# FDA
fitcontrol <- trainControl(method = "repeatedcv",classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           number = 10,
                           repeats = 10)
set.seed(666)
#mtry <- sqrt(ncol(x)) #already have it
metric <- "Accuracy" #not clear why he does that, maybe ust for teaching
FDA <- train(Blank~., data=data, method="fda", metric="ROC", tuneLength=20, trControl=fitcontrol) #still not clear the tuneLength
print(FDA)
plot(FDA)
confusionMatrix(FDA)

predict(FDA, test)$class


pc_fda<-predict(FDA, train, type = "raw")
pp_fda<-predict(FDA, train, type = "prob")


set.seed(825)
mars <- train(Blank ~ ., data = data,
                         method = "earth", metric = "ROC",
                         tuneLength = 20,
                         trControl = fitcontrol)
confusionMatrix(mars)



library(MASS)
# Fit the model
model_fda <- fda(Blank~., data = train)
# Make predictions

confusion_fda <- confusion(predict(model_fda, test), test$Blank)

predict(model_fda, test)

predictions_fda <- model_fda %>% predict.fda(test)

plot(model_fda)

names(predictions_fda)

# Model accuracy
mean(predictions_fda$class==test$Blank)





