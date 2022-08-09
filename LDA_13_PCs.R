PCs_3d_90 <- read.csv("PCs_3d.csv")

Retouch <- Dataset_new_plus_sections %>%
  select(ID, Retouch)

PCs_3d_90 <- left_join(PCs_3d_90, Retouch, by = "ID")


PCs_3d_90_rf <- PCs_3d_90 %>%
  select(-ID) %>%
  mutate(Retouch = recode(Retouch, `Direct bilateral` = "Direct.bilateral", `Direct unilateral` = "Direct.unilateral"))


PCs_3d_90_rf$Retouch <- as.factor(PCs_3d_90_rf$Retouch)

mtry_3d_90 <- sqrt(ncol(PCs_3d_90_rf))

#-- no split
set.seed(666)
PCs_3d_90_rf_no_split <- randomForest(Retouch~., data=PCs_3d_90_rf, localImp = TRUE, ntree = 1500, mtry = mtry_3d, proximity=TRUE)
print(rf_3d_no_split)

varImpPlot(PCs_3d_90_rf_no_split,
           sort = T,
           n.var = 9,
           main = "Variable Importance")


#LDA--
PCs_3d_90_lda <- PCs_3d_90_rf %>%
  select("Retouch", "PC3", "PC5", "PC2", "PC4")

set.seed(666)
linear_PCs_90 <- lda(Retouch~., PCs_3d_90_lda)
linear_PCs_90 #call it

attributes(linear_PCs_90)


p_90_lda <- predict(linear_PCs_90, PCs_3d_90_lda)
ldahist(data = p_90_lda$x[,1], g = PCs_3d_90_lda$Retouch) #the 1 is LD1, if you hads more you should also testthe LD2 and so on, but here there are only 2 groups

ggord(linear_PCs_90, PCs_3d_90_lda$Retouch, ylim = c(-5, 5)) #I cannot do that because O do not have a LD2

partimat(Retouch~., data = train_LDA, method = "lda") #Not works, maybe too many predictors

partimat(Blank~., data = train, method = "qda", plot.matrix = T, imageplot = F)

p1_90_LDA <- predict(linear_PCs_90, PCs_3d_90_lda)$class
tab_90_LDA <- table(Predicted = p1_90_LDA, Actual = PCs_3d_90_lda$Retouch)
tab_90_LDA

p1_prob <- predict(linear, train, type = "prob") #removed the $class (see why it's used)
p1_raw <- predict(linear, train, type = "raw")$class

p1_prob$posterior

sum(diag(tab_90_LDA))/sum(tab_90_LDA) # proportion of accuracy

p2_LDA <- predict(linear, test_LDA)$class
tab_LDA_test <- table(Predicted = p2_LDA, Actual = test_LDA$Retouch)
tab_LDA_test

sum(diag(tab_LDA_test))/sum(tab_LDA_test)











set.seed(666) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_3d_90 <- sample(2, nrow(PCs_3d_90_rf), replace = TRUE, prob = c(0.7, 0.3))
train_3d_90 <- PCs_3d_90_rf[ind_3d_90==1,]
test_3d_90 <- PCs_3d_90_rf[ind_3d_90==2,]


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
