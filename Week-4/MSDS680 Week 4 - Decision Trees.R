library(DataExplorer)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(plyr)

set.seed(486)

wine_data <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', sep=';', header = T)

head(wine_data, 10)
col_names <- c('fx.acid', 'vol.acid', 'ctrc.acid', 'res.sug', 
                'chlor', 'free.so2', 'tot.so2', 'dens', 
                'pH', 'sulph', 'alc', 'qual')
names(wine_data) <- col_names

summary(wine_data)
plot_correlation(wine_data)
count(wine_data, vars = 'qual')



wine_data$qual[wine_data$qual == 3] <- 4
wine_data $qual[wine_data$qual == 8] <- 7
count(wine_data, vars = 'qual')

wine_data$qual <- as.factor(wine_data$qual)
idx = createDataPartition(wine_data$qual, p=.7, list=F)

wine_train = wine_data[idx,]
wine_test = wine_data[-idx,]
prop.table(table(wine_train$qual))
prop.table(table(wine_test$qual))

wine_tree <- rpart(qual ~ ., data=wine_train, cp = 0)
printcp(wine_tree) 
plotcp(wine_tree)
rpart.plot(wine_tree, branch = .3)

wine_pred <- predict(wine_tree, wine_test, type='class')
confusionMatrix(wine_pred, wine_test$qual)


min(wine_tree$cptable[,'xerror'])
which.min(wine_tree$cptable[,'xerror'])
wine_cp <- wine_tree$cptable[which.min(wine_tree$cptable[,'xerror']),'CP']

pruned_wine <- prune(wine_tree, cp = wine_cp)
printcp(pruned_wine) 
rpart.plot(pruned_wine, branch = .3)

pruned_pred <- predict(pruned_wine, wine_test, type='class')
confusionMatrix(pruned_pred, wine_test$qual)

wine_forest <- randomForest(qual ~ ., wine_train, ntree=100, importance=T)
varImpPlot(wine_forest)

forest_pred <- predict(wine_forest, wine_test, type='class')
confusionMatrix(forest_pred, wine_test$qual)
