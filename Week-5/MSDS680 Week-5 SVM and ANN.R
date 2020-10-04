library(data.table) #used to read in data
library(DataExplorer)#used to inspect the data and get initial understanding
library(e1071) #used for SVM model
library(caret) #used for model analysis
library(class) #used for converting data types
library(dbplyr) #used for removing/adding columns
library(neuralnet) #used for neural net model

set.seed(476)

data <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data', header = F, sep = ',')

col_names <- c('class', 'cap.shape', 'cap.surface', 'cap.clr', 'bruises', 'odor',
               'gill.attach', 'gill.spacing', 'gill.sz', 'gill.clr',
               'stalk.shape', 'stalk.root', 'stalk.sar', 'stalk.sbr',
               'stalk.clr.abv.rng', 'stalk.clr.bel.rng', 'veil.type', 'veil.clr',
               'ring.num', 'ring.type', 'spore.prnt.clr', 'pop', 'habitat')

names(mushroom_data) <- col_names
# initial data inspection
head(mushroom_data)
summary(mushroom_data)
plot_intro(mushroom_data)
plot_missing(mushroom_data)
plot_correlation(mushroom_data)

shroom_dat <- subset(mushroom_data, select = -c(`veil.type`))
levels(shroom_dat$`stalk.root`)[levels(shroom_dat$`stalk.root`)=="?"] <- "u" #converts missing data to "unknown"

shroom_dat$class <- as.factor(shroom_dat$class)
str(shroom_dat)


idx <- createDataPartition(shroom_dat$class, p=0.7, list=FALSE) #creates index to split data

shroom_train <- shroom_dat[idx,]
shroom_test <- shroom_dat[-idx,] 

svm_linear <- svm(class~., data=shroom_train, type='C-classification', kernel='linear') # applies svm with a linear fit
shroom_lin_pred <- predict(svm_linear, shroom_test)
svm_radial <- svm(class~., data=shroom_train, type='C-classification', kernel='radial') # applies svm with a radial fit
shroom_rad_pred <- predict(svm_radial, shroom_test)
svm_poly <- svm(class~., data=shroom_train, type='C-classification', kernel='polynomial') # applies svm with a polynomial fit
shroom_poly_pred <- predict(svm_poly, shroom_test)
svm_sigmoid <- svm(class~., data=shroom_train, type='C-classification', kernel='sigmoid') # applies svm with a sigmoid fit
shroom_sig_pred <- predict(svm_sigmoid, shroom_test)

confusionMatrix(shroom_lin_pred, shroom_test$class)$table 
confusionMatrix(shroom_rad_pred, shroom_test$class)$table
confusionMatrix(shroom_poly_pred, shroom_test$class)$table
confusionMatrix(shroom_sig_pred, shroom_test$class)$table


shroom_features <- subset(shroom_dat, select = -class) #removing class to dummy variable for ANN
shroom_Dummy <- dummyVars(~., data=shroom_features) #applies levels to variables for splitting factors
shroom_Dummy <- data.frame(predict(shroom_Dummy, shroom_features)) #transforms data into boolean columns for each level of factors
shroom_Dummy$class <- shroom_dat$class
ncol(shroom_Dummy)

idx <- createDataPartition(shroom_Dummy$class, p=0.7, list=FALSE) #creates index to split data
ann_shroom_train <- shroom_Dummy[idx,]
ann_shroom_test <- shroom_Dummy[-idx,]
training_cols <- names(subset(shroom_Dummy, select = -c(class))) 

nn.formula <- as.formula(paste("class ~ ", paste(training_cols, collapse = " + "))) #combines all of the columns with +s to add into the ANN model
network <- neuralnet(nn.formula, shroom_Dummy) #creates neural network on dummy variable dataset
plot(network)

net_predict <- compute(network, subset(ann_shroom_test, select = -c(`class`)))$net.result
net_predict <- as.factor(c('e','p')[apply(net_predict, 1, which.max)]) #converts results from numeric to e and p to match factor of dataset

confusionMatrix(net_predict, ann_shroom_test$class)

shroom_numeric <- shroom_dat #replicates data for lewss features
shroom_numeric[, 2:22] <- sapply(shroom_numeric[,2:22],as.numeric) # converts all columns to numeric

num_idx <- createDataPartition(shroom_Dummy$class, p=0.7, list=FALSE) #creates index to split data
num_train <- shroom_numeric[num_idx,]
num_test <- shroom_numeric[-num_idx,]
num_train_names <- names(subset(shroom_numeric, select = -c(class)))

numeric_formula <- as.formula(paste("class ~ ", paste(num_train_names, collapse = " + ")))
num_network <- neuralnet(numeric_formula, shroom_numeric) #performs neural network using original features as numeric columns
plot(num_network)

num_predict <- compute(num_network, subset(num_test, select = -c(class)))$net.result 
num_predict <- as.factor(c('e','p')[apply(num_predict, 1, which.max)])

confusionMatrix(num_predict, num_test$class)

num_network_2 <- neuralnet(numeric_formula, shroom_numeric, hidden=3) #repeats network with added hidden nodes 
plot(num_network_2)

num_predict_2 <- compute(num_network_2, subset(num_test, select = -c(class)))$net.result 
num_predict_2 <- as.factor(c('e','p')[apply(num_predict_2, 1, which.max)])

confusionMatrix(num_predict_2, num_test$class)

num_network_3 <- neuralnet(numeric_formula, shroom_numeric, hidden=c(5,3)) #repeats network with added hidden nodes and layers
plot(num_network_3)

num_predict_3 <- compute(num_network_3, subset(num_test, select = -c(class)))$net.result 
num_predict_3 <- as.factor(c('e','p')[apply(num_predict_3, 1, which.max)])

confusionMatrix(num_predict_3, num_test$class)