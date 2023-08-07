library(readxl)
library('dplyr') #for Selecting columns by list of index
library(NbClust)  #for determining the relevant number of clusters
library(factoextra)  #for determining the relevant number of clusters
library(cluster)   #for silhouette analysis
library('fpc') #for Calinski-Harabasz Index



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
  
  #Print the detected outliers
  cat("Detected Outliers in", vari, ":", outliers, "\n")
  
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



# Apply PCA method

pca_result <- prcomp(vehicles, scale = FALSE)
summary(pca_result)


#each column of pca_result$rotation contains the corresponding principal component loading vector
pca_result$rotation <- -pca_result$rotation #by default, eigenvectors in R point in the negative direction
pca_result$rotation


# obtain the principal components scores 
pca_result$x <- - pca_result$x
head(pca_result$x)


# since cumulative score > 92%, new transformed dataset have 6 PCs as attributes
transformed_data <- pca_result$x[,1:6]
head(transformed_data)




#Determine the number of cluster centres
set.seed(26)

# NbClust method
clusterNo_method1=NbClust(transformed_data,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# Elbow Method
fviz_nbclust(transformed_data, kmeans, method = 'wss')

# Gap Statistic method
fviz_nbclust(transformed_data, kmeans, method = 'gap_stat')

# Silhouette Method
fviz_nbclust(transformed_data, kmeans, method = 'silhouette')


# numbr of clusters
k2 = 2
k3 = 3


# kmeans clustering
kmeans_vehicles_k2 = kmeans(transformed_data, centers = k2, nstart = 10)
kmeans_vehicles_k2
kmeans_vehicles_k3 = kmeans(transformed_data, centers = k3, nstart = 10)
kmeans_vehicles_k3



# Internal evaluation k = 2
wss_k2 = kmeans_vehicles_k2$tot.withinss 
bss_k2 = kmeans_vehicles_k2$betweenss
wss_k2
bss_k2

# Internal evaluation k = 3
wss_k3 = kmeans_vehicles_k3$tot.withinss 
bss_k3 = kmeans_vehicles_k3$betweenss
wss_k3
bss_k3




# The silhouette analysis 
sil <- silhouette(kmeans_vehicles_k3$cluster, dist(transformed_data))
fviz_silhouette(sil)



# Calinski-Harabasz Index
ch_values <- NULL
for (i in 1:10) {
  kmeans_vehicles = kmeans(transformed_data, centers = i, nstart = 10)
  ch <- calinhara(transformed_data,kmeans_vehicles$cluster)
  ch_values <- c(ch_values,ch)
  
}
# plot the Calinski-Harabasz Index values for different k values.
plot(1:10, ch_values, type = "b", xlab = "Number of Clusters", ylab = "Calinski-Harabasz Index")

max(ch_values[2])
