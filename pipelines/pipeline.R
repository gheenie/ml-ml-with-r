# Syntax for creating lists.

u <- c(-10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0)
u <- -10:0
u <- seq(-10, 0)
u <- seq(from=-10, to=0, by=1)
u <- seq(from=-10, to=0, length.out=11)
print(u)
v <- seq(-0.1, by=0.5, length=11)
print(v)

# List operations.

u + v # No need print()
u * v # A bit unintuitive - element-wise multiplication.
0.8 * u

w <- c(u, v)
print(w)
w[14:16]
w[c(2, 5, 9, 21)]
w[23]

# Some functions.

sort(w, decreasing=TRUE)

# More list syntax.

bMatrix <- matrix(
  seq(1, 39, by=2),
  4,
  5,
  byrow=T,
  dimnames=list(c('A', 'B', 'C', 'D'), c('a', 'b', 'c', 'd', 'e')) # list vs c
)
bMatrix

subB <- bMatrix[c(1, 2, 4), c(2, 3)]
subB

# Assignment to functions.

x <- c(1, 2, 3)
y <- c(4, 5, 6)
z <- c(7, 8, 9)
A <- cbind(x, y, z)
rownames(A) <- c('a', 'b', 'c')
colnames(A) <- c('X', 'Y', 'Z')
A
# If combined by rows:
B <- rbind(x, y, z)
B

# Functions for descriptive stats.

x <- c(10, 2, 15, 6, 4, 9, 12, 11, 3, 0, 12, 10, 9, 7, 11, 10, 8, 5, 10, 6)
length(x)
sum(x)
mean(x)
median(x)
# Mode.
# table(), -, and sort() are overloaded. All applied to frequency column.
names(sort(-table(x)))[1]
summary(x)
var(x)
sd(x)

set.seed(63) # One-time use only.
y <- rnorm(20, mean(x), 1)
cov(x, y)
cor(x, y)

# Functions for visualisations.

set.seed(100)
z <- rnorm(20, 8, 1)
plot(
  z ~ x,
  main='Scatter plot of x and z',
  xlab='x',
  ylab='z',
  ylim=c(-10, 40)
)

set.seed(100)
u <- rnorm(20, 8, 10)
# points() adds points to the existing plot. Global variables.
points(u ~ x, col='blue', pch='+')

boxplot(x)

data <- data.frame(x, y)
boxplot(
  data,
  col=c('green', 'orange'),
  main='Boxplot for x and y',
  names=c('Green X', 'Orange Y')
)

# Linear reg functions.

damage <- c(
  26.2, 17.8, 31.3, 23.1, 27.5, 36.0, 14.1, 22.3, 19.6, 31.3, 24.0, 17.3, 43.2,
  36.4, 26.1
)
distance <- c(
  3.4, 1.8, 4.6, 2.3, 3.1, 5.5, 0.7, 3.0, 2.6, 4.3, 2.1, 1.1, 6.1, 4.8, 3.8
)
fireDamage <- data.frame(damage, distance)
# The modelling.
lm.fit <- lm(damage ~ distance, fireDamage)
lm.fit
summary(lm.fit)

plot(
  damage ~ distance,
  xlab='distance',
  ylab='damage',
  main='Plot of damage against distance'
)
# Plotting the model.
abline(lm.fit)

# The predicting using the model.
predict(
  lm.fit,
  newdata=data.frame(distance=c(0.5, 3, 5.5)),
  interval='prediction'
)
predict(
  lm.fit,
  newdata=data.frame(distance=c(0.5, 3, 5.5)),
  interval='confidence'
)

plot(
  damage ~ distance,
  xlab='distance',
  ylab='damage',
  main='Confidence intervals and prediction intervals',
  ylim=c(10,50)
)
abline(lm.fit)
newDist <- data.frame(distance=seq(0.75, 6.0, length=51))
newConfidenceInterval <- predict(lm.fit, newDist, interval='confidence')
newPredictionInterval <- predict(lm.fit, newDist, interval='prediction')
# Plotting the prediction.
lines(
  newConfidenceInterval[, 'lwr'] ~ newDist$distance,
  col='red',
  pch='+',
  type='b'
)
lines(
  newConfidenceInterval[, 'upr'] ~ newDist$distance,
  col='red',
  pch='+',
  type='b'
)
lines(
  newPredictionInterval[, 'lwr'] ~ newDist$distance,
  col='blue',
  pch='*',
  type='b'
)
lines(
  newPredictionInterval[, 'upr'] ~ newDist$distance,
  col='blue',
  pch='*',
  type='b'
)
legend(
  'bottomright',
  pch=c('+', '*'),
  col=c('red', 'blue'),
  legend=c('confidence', 'interval')
)

# Installation processes.

# Work in an RStudio project first.
# install.packages('renv')
# renv::init()
# renv::install('aod')
library(aod)

# More functions for descriptive stats.

myData <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(myData)
myData$admit
nrow(myData)
View(myData)
summary(myData)
# Get the sd for each column.
sapply(myData, sd)
# Get the mean for all rows, each column except 4th.
sapply(myData[, -4], mean)

# Logistic reg functions.

myData$rank <- factor(myData$rank)
myData$rank

# The modelling.
glmFitAdmit <- glm(admit ~ gre+gpa+rank, data=myData, family='binomial')
summary(glmFitAdmit)
# The predicting using the model.
# Default data is the dataset passed into glm when fitting.
admitProb <- predict(glmFitAdmit, type='response')
admitProb
# Set a threshold to convert to a predicted class.
admitPred <- rep(1, 400)
admitPred[admitProb < 0.5] <- 0
admitPred
# The evaluation.
table(admitPred, myData$admit)
mean(admitPred == myData$admit)

meanData <- with(
  myData,
  data.frame(gre=mean(gre), gpa=mean(gpa), rank=factor(1:4))
)
meanData

meanData$admitProb <- predict(glmFitAdmit, newdata=meanData, type='response')
meanData
meanData$admitPred <- rep(1, 4)
meanData$admitPred[meanData$admitProb < 0.5] <- 0
meanData

# Shortlisting variables and model evaluation.

renv::install('caret')
renv::install('ROCR')
library(caret)
library(ROCR)

trainingData <- read.table('WineQuality_training.txt', header=TRUE, sep=',')
View(trainingData)
summary(trainingData)
trainingData[, 3] <- as.factor(trainingData[, 3])
set.seed(11)
folds <- createFolds(y=trainingData[, 3], k=10)
str(folds)
aurocsAlcohol <- as.numeric()
aurocsSugar <- as.numeric()
aurocsAlcoholSugar <- as.numeric()

# k-fold cross validation.
for (i in 1:10) {
  foldTrain <- trainingData[-folds[[i]],]
  foldTest <- trainingData[folds[[i]],]
  # Modelling.
  modelAlcohol <- glm(quality ~ alcohol, data=foldTrain, family='binomial')
  modelSugar <- glm(quality ~ residual.sugar, data=foldTrain, family='binomial')
  modelAlcoholSugar <- glm(
    quality ~ alcohol+residual.sugar,
    data=foldTrain,
    family='binomial'
  )
  # Predicting.
  probAlcohol <- predict(modelAlcohol, foldTest, type='response')
  probSugar <- predict(modelSugar, foldTest, type='response')
  probAlcoholSugar <- predict(modelAlcoholSugar, foldTest, type='response')
  predAlcohol <- prediction(probAlcohol, foldTest$quality)
  predSugar <- prediction(probSugar, foldTest$quality)
  predAlcoholSugar <- prediction(probAlcoholSugar, foldTest$quality)
  # Evaluation.
  aurocAlcohol <- performance(predAlcohol, measure='auc')
  aurocSugar <- performance(predSugar, measure='auc')
  aurocAlcoholSugar <- performance(predAlcoholSugar, measure='auc')
  # @ accesses class attributes. Single brackets returns a list of elements.
  # Double brackets accesses elements.
  aurocsAlcohol <- append(aurocsAlcohol, aurocAlcohol@y.values[[1]])
  aurocsSugar <- append(aurocsSugar, aurocSugar@y.values[[1]])
  aurocsAlcoholSugar <- append(
    aurocsAlcoholSugar,
    aurocAlcoholSugar@y.values[[1]]
  )
}
mean(aurocsAlcohol)
mean(aurocsSugar)
mean(aurocsAlcoholSugar)

testingData <- read.table('WineQuality_testing.txt', header=TRUE, sep=',')
dim(testingData)
testingData[, 3] <- as.factor(testingData[, 3])

# Train-test validation.
modelAlcohol <- glm(quality ~ alcohol, data=trainingData, family='binomial')
modelSugar <- glm(
  quality ~ residual.sugar,
  data=trainingData,
  family='binomial'
)
modelAlcoholSugar <- glm(
  quality ~ alcohol+residual.sugar,
  data=trainingData,
  family='binomial'
)
probAlcohol <- predict(modelAlcohol, testingData, type='response')
probSugar <- predict(modelSugar, testingData, type='response')
probAlcoholSugar <- predict(modelAlcoholSugar, testingData, type='response')
predAlcohol <- prediction(probAlcohol, testingData$quality)
predSugar <- prediction(probSugar, testingData$quality)
predAlcoholSugar <- prediction(probAlcoholSugar, testingData$quality)
aurocAlcohol <- performance(predAlcohol, measure='auc')@y.values[[1]]
aurocSugar <- performance(predSugar, measure='auc')@y.values[[1]]
aurocAlcoholSugar <- performance(predAlcoholSugar, measure='auc')@y.values[[1]]
aurocAlcohol
aurocSugar
aurocAlcoholSugar
tprfprAlcohol <- performance(predAlcohol, measure='tpr', x.measure='fpr')
tprfprSugar <- performance(predSugar, measure='tpr', x.measure='fpr')
tprfprAlcoholSugar <- performance(
  predAlcoholSugar,
  measure='tpr',
  x.measure='fpr'
)
plot(tprfprAlcohol, col='blue')
plot(tprfprSugar, col='orange', add=TRUE)
plot(tprfprAlcoholSugar, col='red', add=TRUE)
legend(
  0.35,
  0.25,
  c(
    text=sprintf('AUROC(alcohol) = %s', round(aurocAlcohol, digits=3)),
    text=sprintf('AUROC(residual.sugar) = %s', round(aurocSugar, digits=3)),
    text=sprintf(
      'AUROC(alcohol+residual.sugar) = %s',
      round(aurocAlcoholSugar, digits=3)
    )
  ),
  lty=1,
  cex=0.9,
  col=c("blue", "orange", "red"),
  bty="n",
  y.intersp=1,
  inset=c(0.1, -0.15)
)

# Decision trees functions.

renv::install('readr')
renv::install('tree')
library(readr)
library(dplyr)
library(tree)

# %>% pipes into
titanic <- "https://goo.gl/At238b" %>%
  read_csv %>%
  select(survived, embarked, sex, sibsp, parch, fare) %>%
  mutate(survived=factor(survived), embarked=factor(embarked), sex=factor(sex))
titanic

# Model on all observations.
treeTitanic <- tree(survived ~ embarked+sex+sibsp+parch+fare, titanic)
plot(treeTitanic)
text(treeTitanic, pretty=0)
# Evaluation.
summary(treeTitanic)

set.seed(2)
titanicTrainIndices <- sample(1:nrow(titanic), nrow(titanic)/2)
titanicTest <- titanic[-titanicTrainIndices,]
survivedTest <- titanic$survived[-titanicTrainIndices]

# Model on train-split observations.
treeTitanicTrain <- tree(
  survived ~ embarked+sex+sibsp+parch+fare,
  titanic,
  subset=titanicTrainIndices
)
plot(treeTitanicTrain)
text(treeTitanicTrain, pretty=0)
# Predict.
titanicPred <- predict(treeTitanicTrain, titanicTest, type='class')
# Evaluate.
mean(titanicPred != survivedTest)
table(titanicPred, survivedTest)

set.seed(21)
cvTreeTitanicTrain <- cv.tree(treeTitanicTrain, FUN=prune.misclass)
cvTreeTitanicTrain

# Pruning.
# Model.
prunedTitanicTrain <- prune.misclass(treeTitanicTrain, best=4)
plot(prunedTitanicTrain)
text(prunedTitanicTrain, pretty=0)
# Predict.
titanicPred <- predict(prunedTitanicTrain, titanicTest, type='class')
# Evaluate.
mean(titanicPred != survivedTest)

# Functions for random forests with bagging.

renv::install('randomForest')
library(randomForest)

titanic <- 'titanic3.csv' %>%
  read_csv %>%
  select(pclass, survived, sex, age, sibsp, parch, fare, embarked) %>%
  mutate(
    pclass=factor(pclass),
    sex=factor(sex),
    embarked=factor(embarked),
    survivedFactor=factor(survived)
  )
summary(titanic)
nrow(titanic)
titanic <- na.omit(titanic)
nrow(titanic)
set.seed(1)
trainIndices <- sample(1:nrow(titanic), nrow(titanic)/2)
test <- titanic[-trainIndices,]
xTest <- test[, -c(2,9)]
survivedTest <- titanic$survived[-trainIndices]
survivedFactorTest <- titanic$survivedFactor[-trainIndices]

set.seed(1)
modelRegression <- randomForest(
  survived ~ .-survivedFactor,
  data=titanic,
  subset=trainIndices,
  mtry=7,
  importance=TRUE
)
modelRegression
prob <- predict(modelRegression, newdata=test)
pred <- ifelse(prob<=0.5, '0', '1')
mean(pred != survivedTest)
set.seed(1)
modelClassification <- randomForest(
  survivedFactor ~ .-survived,
  data=titanic,
  subset=trainIndices,
  mtry=7,
  importance=TRUE
)
modelClassification
pred <- predict(modelTitanicFactor, newdata=test, type='class')
mean(pred != survivedFactorTest)
# More evaluation.
importance(modelRegression)
varImpPlot(modelRegression)
importance(modelClassification)
varImpPlot(modelClassification)
barplot(
  sort(importance(modelRegression)[, 1], decreasing=TRUE),
  xlab="Relative Importance",
  horiz=TRUE,
  col="red",
  las=1 # Allow rotation of 90 degrees for labels
)

# Vary number of trees.
set.seed(1)
modelClassification <- randomForest(
  survivedFactor ~ .-survived,
  data=titanic,
  subset=trainIndices,
  importance=TRUE,
  xtest=xTest,
  ytest=survivedFactorTest,
  mtry=7,
  ntree=200
)
modelClassification$test$err.rate[200, 1]
plot(
  1:200,
  modelClassification$test$err.rate[, 1],
  type='l',
  xlab='Number of Bootstrap Data Sets',
  ylab='Testing Error Rate',
  xlim=c(0, 205),
  ylim=c(0.17, 0.32)
)
abline(
  h=modelClassification$test$err.rate[1, 1],
  lty=2,
  col='red'
)
errorRates <- rep(0, 200)
for (i in 1:200) {
  set.seed(6)
  modelRegression <- randomForest(
    survived ~ .-survivedFactor,
    data=titanic,
    subset=trainIndices,
    mtry=7,
    importance=TRUE,
    ntree=i
  )
  prob <- predict(modelRegression, newdata=test)
  pred <- ifelse(prob<=0.5, '0', '1')
  errorRates[i] <- mean(pred != survivedTest)
}
lines(errorRates, col='blue')
legend(
  'topright',
  c('one tree', 'majority vote', 'averaging probs'),
  lty=c(2, 1, 1),
  col=c('red', 'black', 'blue')
)

# Vary number of random variables.
errorRates <- rep(0, 7)
for (i in 1:7) {
  set.seed(3)
  modelClassification <- randomForest(
    survivedFactor ~ .-survived,
    data=titanic,
    subset=trainIndices,
    importance=TRUE,
    xtest=xTest,
    ytest=survivedFactorTest,
    mtry=i,
    ntree=500
  )
  errorRates[i] <- modelClassification$test$err.rate[500, 1]
}
plot(errorRates, type='b', xlab='mtry', ylab='Testing Error Rate')

# Showing best number of random variables.
plot(
  0,
  xlab='Number of Trees',
  ylab='Testing Error Rate',
  xlim=c(1, 540),
  ylim=c(0.18, 0.28)
)
for (i in 1:7) {
  set.seed(3)
  modelClassification <- randomForest(
    survivedFactor ~ .-survived,
    data=titanic,
    subset=trainIndices,
    importance=TRUE,
    xtest=xTest,
    ytest=survivedFactorTest,
    mtry=i,
    ntree=500
  )
  lines(
    modelClassification$test$err.rate[, 1],
    col=i,
    type='l'
  )
}
legend(
  title='mtry',
  'topright',
  c('1', '2', '3', '4', '5', '6', '7'),
  lty=rep(1, 7),
  col=1:7
)

# Functions for SVM.

library(e1071)

# Linearly separable dataset.
set.seed(300)
xPos <- matrix(rnorm(100*2, mean=0), nrow=100, ncol=2)
set.seed(300)
xNeg <- matrix(rnorm(100*2, mean=3), nrow=100, ncol=2)
x <- rbind(xPos, xNeg)
y <- c(rep(1, 100), rep(-1, 100))
plot(
  x,
  col=ifelse(y>0, 1, 2)
)
legend('topleft', c('Positive', 'Negative'), text.col=seq(2), pch=1, col=seq(2))
data <- data.frame(x=x, y=as.factor(y))
set.seed(300)
trainIndices <- sample(200, 200*0.7)
dataTrain <- data[trainIndices,]
xTest <- x[-trainIndices,]
yTest <- y[-trainIndices]

svmVaryingCost <- function(cost) {
  model <- svm(
    y ~ .,
    data=dataTrain,
    kernel='linear',
    cost=cost,
    scale=FALSE
  )
  print(summary(model))
  plot(model, dataTrain)
  pred <- predict(model, newdata=xTest)
  mean(pred != yTest)
}
svmVaryingCost(1)
svmVaryingCost(0.01)
svmVaryingCost(1e5)

# Evaluate only on training data.
set.seed(1)
tunedSvm <- tune(
  svm,
  y ~ .,
  data=dataTrain,
  kernel='linear',
  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000, 10000, 1e5))
)
print(summary(tunedSvm))

# Linearly inseparable dataset.
set.seed(300)
xPos1 <- matrix(rnorm(30*2, mean=0), nrow=30, ncol=2)
# set.seed(300)
xPos2 <- matrix(rnorm(30*2, mean=3), nrow=30, ncol=2)
set.seed(300)
xNeg1 <- matrix(c(rnorm(30, mean=0)+3, rnorm(30, mean=0)), nrow=30, ncol=2)
# set.seed(300)
xNeg2 <- matrix(c(rnorm(30, mean=3)-3, rnorm(30, mean=3)), nrow=30, ncol=2)
x <- rbind(xPos1, xPos2, xNeg1, xNeg2)
y <- c(rep(1, 60), rep(-1, 60))
plot(
  x,
  col=ifelse(y>0, 1, 2),
  pch=ifelse(y>0, 1, 2)
)
legend(
  'topleft',
  c('Positive', 'Negative'),
  text.col=1:2,
  pch=1:2,
  col=1:2
)
data <- data.frame(x=x, y=as.factor(y))
set.seed(300)
trainIndices <- sample(120, 120*0.7)
dataTrain <- data[trainIndices,]
xTest <- x[-trainIndices,]
yTest <- y[-trainIndices]

svmVaryingCost <- function(cost) {
  model <- svm(
    y ~ .,
    data=dataTrain,
    kernel='radial',
    gamma=1,
    cost=cost,
    scale=FALSE
  )
  print(summary(model))
  plot(model, dataTrain)
  pred <- predict(model, newdata=xTest)
  mean(pred != yTest)
}
svmVaryingCost(1)
svmVaryingCost(1e5)

set.seed(1)
tunedSvm <- tune(
  svm,
  y ~ .,
  data=dataTrain,
  kernel='radial',
  ranges=list(
    cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000, 10000, 1e5),
    gamma=c(0.5, 1, 2, 3, 4)
  )
)
print(summary(tunedSvm))
pred <- predict(tunedSvm$best.model, newdata=xTest)
print(mean(pred != yTest))

# Functions for k-means clustering.

library(datasets)
data <- attitude[, c(3,4)]
plot(
  data,
  main='% of favourable responses to Learning and Privilege',
  pch=20,
  cex=2
)
set.seed(7)
kmeansNstart1 <- kmeans(data, 2, nstart=1)
print(kmeansNstart1$tot.withinss)
plot(
  data,
  col=(kmeansNstart1$cluster + 1),
  main='K-means nstart 1 results with 2 clusters',
  pch=20,
  cex=2
)
set.seed(7)
kmeansNstart100 <- kmeans(data, 2, nstart=100)
print(kmeansNstart100$tot.withinss)
plot(
  data,
  col=(kmeansNstart100$cluster + 1),
  main='K-means nstart 100 results with 2 clusters',
  pch=20,
  cex=2
)
totWithinSSes <- rep(0, 15)
for (i in 1:15) {
  set.seed(70)
  totWithinSSes[i] <- kmeans(data, i, nstart=100)$tot.withinss
}
plot(
  1:15,
  totWithinSSes,
  type='b',
  pch=20,
  cex=2,
  xlab='Number of Clusters',
  ylab='Within group sums of sqaures',
  main='Assessing the Optimal Number of Clusters with the Elbow Method'
)
set.seed(70)
kmeansK6 <- kmeans(data, 6, nstart=100)
plot(
  data,
  col=(kmeansK6$cluster + 1),
  main='K-means k 6 results with 6 clusters',
  pch=20,
  cex=2
)

# Functions for PCA.


