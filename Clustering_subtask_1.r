library(ggplot2)
library(readxl)
library(NbClust)  #for determining the relevant number of clusters
library(factoextra)  #for determining the relevant number of clusters
library(cluster)   #for silhouette analysis


#Read XLSX file
vehicles <- read_excel("vehicles.xlsx")
head(vehicles)


#The structure of data set
str(vehicles)


#Checking missing values
sum(is.na(vehicles))


#checking number of rows and columns (before handling outliers)
dim(vehicles)


#View the outliers using using boxplot
boxplot(vehicles[2:19])



#Generating box plot for each variable in the data set and removing outliers
for (vari in 2:19) {
  
  #Detect outliers using boxplot
  outliers <- boxplot(vehicles[[vari]], plot = FALSE)$out
  
  
  #Remove outliers from the variable in the dataset
  vehicles <- vehicles[!vehicles[[vari]] %in% outliers, , drop = FALSE]
}


#Checking outliers removal
boxplot(vehicles[2:19])



#checking number of rows and columns after removing outliers
dim(vehicles)



#Remove first and last columns
#Scaling data
vehicles <- scale(vehicles[, -c(1, 20)])




#Determine the number of cluster centres
set.seed(26)

# NbClust method
clusterNo_method1=NbClust(vehicles,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# Elbow Method
fviz_nbclust(vehicles, kmeans, method = 'wss')

# Gap Statistic method
fviz_nbclust(vehicles, kmeans, method = 'gap_stat')

# Silhouette Method
fviz_nbclust(vehicles, kmeans, method = 'silhouette')






# kmeans clustering

k = 3
kmeans_vehicles = kmeans(vehicles, centers = k, nstart = 10)
kmeans_vehicles


#Illustration of the clusters
fviz_cluster(kmeans_vehicles, data = vehicles)


# Internal evaluation 
wss = kmeans_vehicles$tot.withinss 
bss = kmeans_vehicles$betweenss

wss
bss




# The silhouette analysis 

sil <- silhouette(kmeans_vehicles$cluster, dist(vehicles))
fviz_silhouette(sil)


