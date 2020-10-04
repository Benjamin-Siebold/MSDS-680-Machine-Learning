library(DataExplorer) #data exploration
library(factoextra) #building wss and silhouette plots
library(tidyverse) #data cleaning
library(cluster) #applies HCA
library(stats) #applies HCA
library(dendextend) #compares dendrograms
library(caret) #dummy variables

setwd('C:/Users/bsieb/Documents/Github/MSDS-680-Machine-Learning/Week-6')
set.seed(422)
customers <- read.csv('Wholesale customers data.csv')

#Data Exploration and Cleaning
summary(customers)
str(customers)
plot_histogram(customers)

customers_factor <- as.data.frame(lapply(customers[,c(1,2)], as.factor))
customers_scaled <- as.data.frame(lapply(customers[,c(3:8)], scale))
dummy_vars <- dummyVars('~.', data=customers_factor,fullRank=T)
dummy_customers <- as.data.frame(predict(dummy_vars, newdata=customers_factor))

clean_customers <- cbind(dummy_customers,customers[,c(3:8)]) #creates dummy dataset without scales
scaled_clean_cust <- cbind(dummy_customers, customers_scaled) #creates scaled dataset with dummy variables

#KMEANS

#compare wss to silhouette for both scaled and unscaled datasets
fviz_nbclust(clean_customers, kmeans, method = 'wss')
fviz_nbclust(scaled_clean_cust, kmeans, method = 'wss')

fviz_nbclust(clean_customers, kmeans, method = 'silhouette')
fviz_nbclust(scaled_clean_cust, kmeans, method = 'silhouette')

fviz_nbclust(clean_customers, kmeans, method = 'wss') +
  geom_vline(xintercept = 5, linetype = 1) #add line to wss to show intended clusters

kmeans_fit <- kmeans(clean_customers, 5)

kmeans_fit$size
kmeans_fit$centers
(kmeans_fit$betweenss/kmeans_fit$totss) #provides fit score

plot(clean_customers, col=kmeans_fit$cluster) #gives all the plots to provide a good candidate for inspection

plot(clean_customers[c('Fresh','Detergents_Paper')], col=kmeans_fit$cluster)
points(kmeans_fit$centers[,c('Fresh', 'Detergents_Paper')], col=1:5, pch=8, cex=2)

clean_customers$kmeans_cluster <- kmeans_fit$cluster

head(clean_customers)

aggregate(clean_customers, list(clean_customers$kmeans_cluster), min)
aggregate(clean_customers, list(clean_customers$kmeans_cluster), max)
aggregate(clean_customers, list(clean_customers$kmeans_cluster), mean)

# HCA

fviz_nbclust(clean_customers, hcut, method  = 'wss') #hca plot with wss
fviz_nbclust(clean_customers, hcut, method  = 'silhouette') #hca plot with silhouette

agg_d <- dist(clean_customers, method = 'euclidean') #agglomerative distance between points using euclidean 

#complete application
hc_complete_agg <- hclust(agg_d, method = 'complete')
plot(hc_complete_agg, cex =.6, hang = -1)
hc_complete_agg_fit <- cutree(hc_complete_agg, k=3)

table(hc_complete_agg_fit)


plot(hc_complete_agg)
rect.hclust(hc_complete_agg, k=3)

#ward.D2 application with recommended clusters
hc_wd2 <- hclust(agg_d, method = 'ward.D2')
plot(hc_wd2, cex =.6, hang = -1)
hc_wd2_fit <- cutree(hc_wd2, k = 3)

table(hc_wd2_fit)

plot(hc_wd2)
rect.hclust(hc_wd2, k = 3, border = 2:5)

fviz_cluster(list(data = clean_customers[,c(1:9)], cluster = hc_wd2_fit))

#ward.D2 application with height lowered
hc_wd2_new_fit <- cutree(hc_wd2, h = 175000)

table(hc_wd2_new_fit)

plot(hc_wd2)
rect.hclust(hc_wd2, h = 175000, border = 2:5)

fviz_cluster(list(data = clean_customers[,c(1:9)], cluster = hc_wd2_new_fit))

#divisive application
div_d <- diana(clean_customers, metric = 'euclidean') #divisive hca

summary(div_d)
plot(div_d, which.plots = 2)

div_cut <- cutree(div_d, k=3)
table(div_cut)
div_d$dc

fviz_cluster(list(data = clean_customers[,c(1:9)], cluster = div_cut))
