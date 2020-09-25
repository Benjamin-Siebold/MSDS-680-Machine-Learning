library(class)
library(gmodels)
library(caret)
library(DataExplorer)
library(data.table)
library(e1071)

heart_data <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data')

col_names <- c('age','sex','cp','trestbps', 'chol', 
               'fbs', 'restecg', 'thalach', 'exang',
               'oldpeak', 'slope', 'ca', 'thal', 'num'
)
names(heart_data) <- col_names
set.seed(476)

summary(heart_data))



heart_data$ca <- as.numeric(heart_data$ca)
heart_data$thal <- as.numeric(heart_data$thal)
plot_intro(heart_data)
plot_correlation(heart_data)
summary(heart_data)
plot_missing(heart_data)

heart_no_na <- na.omit(heart_data)

head(heart_no_na,10)

plot_correlation(heart_no_na)

scaled_heart_cols <- as.data.frame(lapply(heart_no_na[,c(1,4,5,8,10)], scale))
factor_heart_cols <- as.data.frame(lapply(heart_no_na[,c(3,7,11:13)],as.factor))
dummy_heart <- dummyVars(~., data=factor_heart_cols,fullRank=TRUE)
dummy_heart_cols <- as.data.frame(predict(dummy_heart, newdata=factor_heart_cols))

heart_no_na$num <- ifelse(heart_no_na$num >= 1,1,0)
heart_no_na$pred <- as.factor(heart_no_na$num)
rest_heart_columns <- heart_no_na[,c(2,6,9,15)]

clean_heart <- cbind(scaled_heart_cols, dummy_heart_cols, rest_heart_columns)

idx <- createDataPartition(clean_heart$pred, p=0.7, list=FALSE)

heart.train <- (clean_heart[idx,])
heart.test <- (clean_heart[-idx,])
train.labels <- heart.train
test.labels <- heart.test$pred

prediction <- knn(heart.train, heart.test, heart.train$pred, k=1)

f1_scores <- list()
max_f1 = 0
k_opt = 0

for (i in c(1:20)){
  prediction <- knn(heart.train, heart.test, heart.train$pred, k=i)
  c <- confusionMatrix(prediction, test.labels)
  f1 <- as.numeric(c$byClass['F1'])
  f1_scores[[i]] <- f1
  if (f1>max_f1){
    max_f1 <- f1
    k_opt <- i
  }         
}

plot(c(1:20), f1_scores)

knn_range <- function(data) {
  set.seed(476)
  neighbor_range <- c(1:(ncol(data) - 1))
  f1_scores <- list()
  max_f1 = 0
  k_opt = 0
  idx <- createDataPartition(data$pred, p=0.7, list=FALSE)
  heart.train <- (data[idx,])
  heart.test <- (data[-idx,])
  train.labels <- heart.train$pred
  test.labels <- heart.test$pred
  for (i in neighbor_range){
    prediction <- knn(heart.train, heart.test, heart.train$pred, k=i)
    c <- confusionMatrix(prediction, test.labels)
    f1 <- as.numeric(c$byClass['F1'])
    f1_scores[[i]] <- f1
    if (f1>max_f1){
      max_f1 <- f1
      k_opt <- i
    }       
  }
  return(plot(neighbor_range, f1_scores))
}

knn_range(clean_heart)

set.seed(476)
cidx <- createDataPartition(clean_heart$pred, p=0.7, list=FALSE)
cheart.train <- (clean_heart[idx,])
cheart.test <- (clean_heart[-idx,])
ctrain.labels <- cheart.train$pred
ctest.labels <- cheart.test$pred
cprediction <- knn(cheart.train, cheart.test, ctrain.labels, k=5) ###replace this value
c <- confusionMatrix(cprediction, ctest.labels)
c
as.numeric(c$byClass['F1'])

reduced_heart <- subset(clean_heart, select= -c(fbs,chol))
knn_range(reduced_heart)

set.seed(476)
ridx <- createDataPartition(clean_heart$pred, p=0.7, list=FALSE)
rheart.train <- (reduced_heart[idx,])
rheart.test <- (reduced_heart[-idx,])
rtrain.labels <- rheart.train$pred
rtest.labels <- rheart.test$pred
rprediction <- knn(rheart.train, rheart.test, rtrain.labels, k=11)
c <-confusionMatrix(rprediction, rtest.labels)
c
as.numeric(c$byClass['F1'])