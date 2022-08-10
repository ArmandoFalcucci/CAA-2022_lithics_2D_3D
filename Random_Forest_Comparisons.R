library(randomForest)
library(caret)
library(MASS)
library(randomForestExplainer)

##3D------

Data_LDA_RF_3d <- Dataset_new_plus_sections %>%
  select(Retouch, PC1_3d, PC2_3d, PC3_3d, PC4_3d, PC1_section, PC2_section, PC3_section, Volume, Mean_angle) %>%
  mutate(Retouch = recode(Retouch, `Direct bilateral` = "Direct.bilateral", `Direct unilateral` = "Direct.unilateral"))


Data_LDA_RF_3d$Retouch <- as.factor(Data_LDA_RF_3d$Retouch)

set.seed(243) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_3d <- sample(2, nrow(Data_LDA_RF_3d), replace = TRUE, prob = c(0.7, 0.3))
train_3d <- Data_LDA_RF_3d[ind_3d==1,]
test_3d <- Data_LDA_RF_3d[ind_3d==2,]


mtry_3d <- sqrt(ncol(Data_LDA_RF_3d)) #to set the mtry as suggested in the package

set.seed(243)
rf_3d <- randomForest(Retouch~., data=train_3d, localImp = TRUE, ntree = 2000, mtry = mtry_3d, proximity=TRUE)
print(rf_3d)

p1_3d <- predict(rf_3d, train_3d)
confusionMatrix(p1_3d, train_3d$Retouch)


p2_3d <- predict(rf_3d, test_3d)
p2_3d_matrix <- confusionMatrix(p2_3d, test_3d$Retouch)


plot(rf_3d)


hist(treesize(rf_3d),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf_3d,
           sort = T,
           n.var = 9,
           main = "Variable Importance")


tiff("RF_importance_3d.tiff", width = 2824, height = 1524, res = 300)
varImpPlot(rf_3d,
           sort = T,
           n.var = 9,
           main = "Variable Importance")
dev.off()


importance(rf_3d)


##2D -------

Data_LDA_RF_2d <- Dataset_new_plus_sections %>%
  select(Retouch, PC1_2d, PC2_2d, PC3_2d, Length, Width, Thickness, Robustness) %>%
  mutate(Retouch = recode(Retouch, `Direct bilateral` = "Direct.bilateral", `Direct unilateral` = "Direct.unilateral"))


Data_LDA_RF_2d$Retouch <- as.factor(Data_LDA_RF_2d$Retouch)

set.seed(243) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_2d <- sample(2, nrow(Data_LDA_RF_2d), replace = TRUE, prob = c(0.7, 0.3))
train_2d <- Data_LDA_RF_2d[ind_2d==1,]
test_2d <- Data_LDA_RF_2d[ind_2d==2,]


mtry <- sqrt(ncol(Data_LDA_RF_2d)) #to set the mtry as suggested in the package

set.seed(243)
rf_2d <- randomForest(Retouch~., data=train_2d, localImp = TRUE, ntree = 2000, mtry = mtry, proximity=TRUE)

print(rf_2d)


p1_2d <- predict(rf_2d, train_2d)
confusionMatrix(p1_2d, train_2d$Retouch)


p2_2d <- predict(rf_2d, test_2d)
p2_2d_matrix <- confusionMatrix(p2_2d, test_2d$Retouch)


varImpPlot(rf_2d,
           sort = T,
           n.var = 7,
           main = "Variable Importance")


tiff("RF_importance_2d.tiff", width = 2824, height = 1524, res = 300)
varImpPlot(rf_2d,
           sort = T,
           n.var = 7,
           main = "Variable Importance")
dev.off()

importance(rf_2d)


## RandomForest using only linear measurements

Data_LDA_RF_no_outline <- Dataset_new_plus_sections %>%
  select(Retouch, Length, Width, Thickness, Robustness, Elongation)


Data_LDA_RF_no_outline$Retouch <- as.factor(Data_LDA_RF_no_outline$Retouch)

set.seed(243) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_no_out <- sample(2, nrow(Data_LDA_RF_no_outline), replace = TRUE, prob = c(0.7, 0.3))
train_no_out <- Data_LDA_RF_no_outline[ind_no_out==1,]
test_no_out <- Data_LDA_RF_no_outline[ind_no_out==2,]


mtry <- sqrt(ncol(Data_LDA_RF_no_outline)) #to set the mtry as suggested in the package

set.seed(243)
rf_no_out <- randomForest(Retouch~., data=train_no_out, localImp = TRUE, ntree = 2000, mtry = mtry, proximity=TRUE)

print(rf_no_out)


p1_no_out <- predict(rf_no_out, train_no_out)
confusionMatrix(p1_no_out, train_no_out$Retouch)


p2_no_out <- predict(rf_no_out, test_no_out)
p2_no_out_matrix <- confusionMatrix(p2_no_out, test_no_out$Retouch)


plot(rf_no_out)


hist(treesize(rf_no_out),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf_no_out,
           sort = T,
           n.var = 5,
           main = "Variable Importance")


tiff("RF_importance_no_out.tiff", width = 2824, height = 1524, res = 300)
varImpPlot(rf_no_out,
           sort = T,
           n.var = 5,
           main = "Variable Importance")
dev.off()


importance(rf_no_out)


## Only 3D shape data -------

PCs_3d_90 <- read.csv("PCs_3d.csv")

Retouch <- Dataset_new_plus_sections %>%
  select(ID, Retouch)

PCs_3d_90 <- left_join(PCs_3d_90, Retouch, by = "ID")

PCs_3d_90_rf <- PCs_3d_90 %>%
  select(-ID) %>%
  mutate(Retouch = recode(Retouch, `Direct bilateral` = "Direct.bilateral", `Direct unilateral` = "Direct.unilateral"))

PCs_3d_90_rf$Retouch <- as.factor(PCs_3d_90_rf$Retouch)

set.seed(243) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind_3d_90 <- sample(2, nrow(PCs_3d_90_rf), replace = TRUE, prob = c(0.7, 0.3))
train_3d_90 <- PCs_3d_90_rf[ind_3d_90==1,]
test_3d_90 <- PCs_3d_90_rf[ind_3d_90==2,]

mtry_3d_90 <- sqrt(ncol(PCs_3d_90_rf)) #to set the mtry as suggested in the package

set.seed(243)
rf_3d_90 <- randomForest(Retouch~., data=train_3d_90, localImp = TRUE, ntree = 2000, mtry = mtry_3d_90, proximity=TRUE)

print(rf_3d_90)


p1_3d_90 <- predict(rf_3d_90, train_3d_90)
confusionMatrix(p1_3d_90, train_3d_90$Retouch)


p2_3d_90 <- predict(rf_3d_90, test_3d_90)
p2_3d_90_matrix <- confusionMatrix(p2_3d_90, test_3d_90$Retouch)


plot(rf_3d_90)


hist(treesize(rf_3d_90),
     main = "No. of Nodes for the Trees",
     col = "green")

varImpPlot(rf_3d_90,
           sort = T,
           n.var = 13,
           main = "Variable Importance")


tiff("RF_importance_3D_shape_only.tiff", width = 2824, height = 1524, res = 300)
varImpPlot(rf_3d_90,
           sort = T,
           n.var = 13,
           main = "Variable Importance")
dev.off()


importance(rf_3d_90)




explain_forest(rf_3d, interactions = TRUE, data = train_3d) #Amnazing way to display the results of the RF with the package randomForestExplainer (https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html)
