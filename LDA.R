#LDA 
#Useful link: https://www.r-bloggers.com/2021/05/linear-discriminant-analysis-in-r/ #

library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)

data <- read.csv("Scores.csv")
str(data)

data$Blank <- as.factor(data$Blank)

table(data$Blank)

set.seed(666) #random seed is to create reproducibility in the code, maybe you can use set.seed(12345)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

linear <- lda(Blank~., train)
linear #call it

attributes(linear)

p <- predict(linear, train)
ldahist(data = p$x[,1], g = train$Blank) #the 1 is LD1, if you hads more you should also testthe LD2 and so on, but here there are only 2 groups

ggord(linear, train$Blank, ylim = c(-10, 10)) #I cannot do that because O do not have a LD2

partimat(Blank~., data = train, method = "lda") #Not works, maybe too many predictors

partimat(Blank~., data = train, method = "qda", plot.matrix = T, imageplot = F)

p1 <- predict(linear, train)$class
tab <- table(Predicted = p1, Actual = train$Blank)
tab

p1_prob <- predict(linear, train, type = "prob") #removed the $class (see why it's used)
p1_raw <- predict(linear, train, type = "raw")$class

p1_prob$posterior

sum(diag(tab))/sum(tab) # proportion of accuracy

p2 <- predict(linear, test)$class
tab1 <- table(Predicted = p2, Actual = test$Blank)
tab1

sum(diag(tab1))/sum(tab1)

