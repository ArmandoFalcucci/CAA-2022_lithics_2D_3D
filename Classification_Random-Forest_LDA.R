library(randomForest)
library(datasets)
library(caret)
library(lattice)
library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
library(dplyr)
library(randomForestExplainer)


randomForest()

Data_LDA_RF_3d <- Dataset_new_plus_sections %>%
  select(Retouch, PC1_3d, PC2_3d, PC3_3d, PC4_3d, PC1_section, PC2_section, PC3_section, Volume, Mean_angle) %>%
  mutate(Retouch = recode(Retouch, `Direct bilateral` = "Direct.bilateral", `Direct unilateral` = "Direct.unilateral"))


Data_LDA_RF_3d$Retouch <- as.factor(Data_LDA_RF_3d$Retouch)

set.seed(666) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_3d <- sample(2, nrow(Data_LDA_RF_3d), replace = TRUE, prob = c(0.7, 0.3))
train_3d <- Data_LDA_RF_3d[ind_3d==1,]
test_3d <- Data_LDA_RF_3d[ind_3d==2,]


mtry_3d <- sqrt(ncol(Data_LDA_RF_3d)) #to set the mtry as suggested in the package

set.seed(666)
rf_3d <- randomForest(Retouch~., data=train_3d, localImp = TRUE, ntree = 1500, mtry = mtry_3d, proximity=TRUE)

print(rf_3d)


explain_forest(rf_3d, interactions = TRUE, data = train_3d) #Amnazing way to display the results of the RF with the package randomForestExplainer (https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html)

#-- no split
set.seed(666)
rf_3d_no_split <- randomForest(Retouch~., data=Data_LDA_RF_3d, localImp = TRUE, ntree = 1500, mtry = mtry_3d, proximity=TRUE)
print(rf_3d_no_split)

p1_3d_no_split <- predict(rf_3d_no_split, Data_LDA_RF_3d)
confusionMatrix(p1_3d_no_split, Data_LDA_RF_3d$Retouch)

varImpPlot(rf_3d_no_split,
           sort = T,
           n.var = 9,
           main = "Variable Importance")
#--


p1_3d <- predict(rf_3d, train_3d)
confusionMatrix(p1_3d, train_3d$Retouch)


p2_3d <- predict(rf_3d, test_3d)
confusionMatrix(p2_3d, test_3d$Retouch)


plot(rf_3d)


hist(treesize(rf_3d),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf_3d,
           sort = T,
           n.var = 9,
           main = "Variable Importance")

importance(rf_3d)


MDSplot(rf_3d, train_3d$Retouch)

MDSplot(rf_3d, test_3d$Retouch)



Data_LDA_RF_2d <- Dataset_new_plus_sections %>%
  select(Retouch, PC1_2d, PC2_2d, PC3_2d, Length, Width, Thickness, Robustness) %>%
  mutate(Retouch = recode(Retouch, `Direct bilateral` = "Direct.bilateral", `Direct unilateral` = "Direct.unilateral"))


Data_LDA_RF_2d$Retouch <- as.factor(Data_LDA_RF_2d$Retouch)

set.seed(666) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_2d <- sample(2, nrow(Data_LDA_RF_2d), replace = TRUE, prob = c(0.7, 0.3))
train_2d <- Data_LDA_RF_2d[ind_2d==1,]
test_2d <- Data_LDA_RF_2d[ind_2d==2,]


mtry <- sqrt(ncol(Data_LDA_RF_2d)) #to set the mtry as suggested in the package

set.seed(666)
rf_2d <- randomForest(Retouch~., data=train_2d, ntree = 1500, mtry = mtry, proximity=TRUE)

print(rf_3d)


p1_2d <- predict(rf_2d, train_2d)
confusionMatrix(p1_2d, train_2d$Retouch)


p2_2d <- predict(rf_2d, test_2d)
confusionMatrix(p2_2d, test_2d$Retouch)


plot(rf_2d)


hist(treesize(rf_2d),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf_2d,
           sort = T,
           n.var = 9,
           main = "Variable Importance")

importance(rf_2d)


MDSplot(rf_3d, train_3d$Retouch)

MDSplot(rf_3d, test_3d$Retouch)




Data_LDA_RF_no_outline <- Dataset_new_plus_sections %>%
  select(Retouch, Length, Width, Thickness, Robustness, Elongation)


Data_LDA_RF_no_outline$Retouch <- as.factor(Data_LDA_RF_no_outline$Retouch)

set.seed(666) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_no_out <- sample(2, nrow(Data_LDA_RF_no_outline), replace = TRUE, prob = c(0.7, 0.3))
train_no_out <- Data_LDA_RF_no_outline[ind_no_out==1,]
test_no_out <- Data_LDA_RF_no_outline[ind_no_out==2,]


mtry <- sqrt(ncol(Data_LDA_RF_no_outline)) #to set the mtry as suggested in the package

set.seed(666)
rf_no_out <- randomForest(Retouch~., data=train_no_out, ntree = 1500, mtry = mtry, proximity=TRUE)

print(rf_no_out)


p1_no_out <- predict(rf_no_out, train_no_out)
confusionMatrix(p1_no_out, train_no_out$Retouch)


p2_no_out <- predict(rf_no_out, test_no_out)
confusionMatrix(p2_no_out, test_no_out$Retouch)


plot(rf_no_out)


hist(treesize(rf_no_out),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf_no_out,
           sort = T,
           n.var = 5,
           main = "Variable Importance")

importance(rf_no_out)


MDSplot(rf_no_out, train_3d$Retouch)

MDSplot(rf_no_out, test_3d$Retouch)


#-----------

Data_LDA <- Dataset_new_plus_sections %>%
  select(Retouch, PC1_section, Mean_angle, PC3_3d) %>%
  mutate(Retouch = recode(Retouch, `Direct bilateral` = "Direct.bilateral", `Direct unilateral` = "Direct.unilateral"))


Data_LDA$Retouch <- as.factor(Data_LDA$Retouch)


set.seed(666) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_LDA <- sample(2, nrow(Data_LDA), replace = TRUE, prob = c(0.7, 0.3))
train_LDA <- Data_LDA[ind_LDA==1,]
test_LDA <- Data_LDA[ind_LDA==2,]

linear <- lda(Retouch~., train_LDA)
linear #call it

attributes(linear)

p_lda <- predict(linear, train_LDA)
ldahist(data = p_lda$x[,1], g = train_LDA$Retouch) #the 1 is LD1, if you hads more you should also testthe LD2 and so on, but here there are only 2 groups

ggord(linear, train_LDA$Retouch, ylim = c(-5, 5)) #I cannot do that because O do not have a LD2

partimat(Retouch~., data = train_LDA, method = "lda") #Not works, maybe too many predictors

partimat(Blank~., data = train, method = "qda", plot.matrix = T, imageplot = F)

p1_LDA <- predict(linear, train_LDA)$class
tab_LDA <- table(Predicted = p1_LDA, Actual = train_LDA$Retouch)
tab_LDA

p1_prob <- predict(linear, train, type = "prob") #removed the $class (see why it's used)
p1_raw <- predict(linear, train, type = "raw")$class

p1_prob$posterior

sum(diag(tab_LDA))/sum(tab_LDA) # proportion of accuracy

p2_LDA <- predict(linear, test_LDA)$class
tab_LDA_test <- table(Predicted = p2_LDA, Actual = test_LDA$Retouch)
tab_LDA_test

sum(diag(tab_LDA_test))/sum(tab_LDA_test)






Data_LDA <- Dataset_new_plus_sections %>%
  select(Retouch, PC1_3d, PC2_3d, PC3_3d, PC4_3d, PC1_section, PC2_section, PC3_section, Volume, Mean_angle) %>%
  mutate(Retouch = recode(Retouch, `Direct bilateral` = "Direct.bilateral", `Direct unilateral` = "Direct.unilateral"))


Data_LDA$Retouch <- as.factor(Data_LDA$Retouch)


set.seed(666) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_LDA <- sample(2, nrow(Data_LDA), replace = TRUE, prob = c(0.7, 0.3))
train_LDA <- Data_LDA[ind_LDA==1,]
test_LDA <- Data_LDA[ind_LDA==2,]

linear <- lda(Retouch~., train_LDA)
linear #call it

attributes(linear)

p_lda <- predict(linear, train_LDA)
ldahist(data = p_lda$x[,1], g = train_LDA$Retouch) #the 1 is LD1, if you hads more you should also testthe LD2 and so on, but here there are only 2 groups

ggord(linear, train_LDA$Retouch, ylim = c(-5, 6.5)) #I cannot do that because O do not have a LD2

partimat(Retouch~., data = train_LDA, method = "lda") #Not works, maybe too many predictors

partimat(Blank~., data = train, method = "qda", plot.matrix = T, imageplot = F)

p1_LDA <- predict(linear, train_LDA)$class
tab_LDA <- table(Predicted = p1_LDA, Actual = train_LDA$Retouch)
tab_LDA

p1_prob <- predict(linear, train, type = "prob") #removed the $class (see why it's used)
p1_raw <- predict(linear, train, type = "raw")$class

p1_prob$posterior

sum(diag(tab_LDA))/sum(tab_LDA) # proportion of accuracy

p2_LDA <- predict(linear, test_LDA)$class
tab_LDA_test <- table(Predicted = p2_LDA, Actual = test_LDA$Retouch)
tab_LDA_test

sum(diag(tab_LDA_test))/sum(tab_LDA_test)

ggord(linear, test_LDA$Retouch, ylim = c(-5, 6.5)) #I cannot do that because O do not have a LD2





