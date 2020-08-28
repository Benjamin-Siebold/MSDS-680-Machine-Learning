#1.
#   1. 1.1) Check the missing values 1.2) remove all rows containing missing values

nrow(airquality)
summary(airquality)
air_means = airquality
air_means$Ozone[is.na(air_means$Ozone)] = round(mean(air_means$Ozone, na.rm=TRUE), 1)
air_means$Solar.R[is.na(air_means$Solar.R)] = round(mean(air_means$Solar.R, na.rm=TRUE), 1)
summary(air_means)

#   2. Impute the missing values with mean or median

air_means = airquality
air_means$Ozone[is.na(air_means$Ozone)] = round(mean(air_means$Ozone, na.rm=TRUE), 1)
air_means$Solar.R[is.na(air_means$Solar.R)] = round(mean(air_means$Solar.R, na.rm=TRUE), 1)
summary(air_means)

#   3. Scale or normalize the data (column)

summary(mtcars)
scaled_cars = mtcars
scaled_cars$mpg = scale(scaled_cars$mpg)
scaled_cars$disp = scale(scaled_cars$disp)
scaled_cars$hp = scale(scaled_cars$hp)
summary(scaled_cars)

#   4. Convert a categorical variable to dummy variables

library(caret)
head(iris)
dummy_vars = dummyVars("~.", data=iris, fullRank=T)
dummy_iris = data.frame(predict(dummy_vars, iris))
head(dummy_iris)

#2. Provide examples of R exploratory functions

library(dplyr)
length(iris)
iris %>% count(Species)

#3. Use the DataExplorer pacakge

library(DataExplorer)
plot_intro(airquality)
plot_missing(airquality)
plot_correlation(na.omit(airquality))
