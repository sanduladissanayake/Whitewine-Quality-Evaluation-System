library(readxl) #load the libraries
library(cluster)
library(NbClust)
library(fpc)
library(factoextra)

#Load white wine dataset
wine_data <- read_excel("Whitewine_v6.xlsx")

# a.Preprocess data and scaling
scaled_data <- scale(wine_data[,1:11]) 

#find the outliers and remove them
outliers <- apply(scaled_data, 1, function(x) any(abs(x) > 3))
updated_data <- wine_data[!outliers, ]

# display the found outliers
removed_outliers <- wine_data[outliers, ] 
print(removed_outliers)


num_of_removed_outliers <- sum(outliers)
cat("Number of outliers removed:", num_of_removed_outliers, "\n")

# b.Determine the number of clusters
nb <- NbClust(updated_data[,1:11],distance = "euclidean", min.nc = 2, max.nc = 10,
              method = "kmeans", index = "all")
print(nb)

# Elbow Method
fviz_nbclust(updated_data[,1:11], kmeans, method = "wss") + labs(title = "Elbow Method")

# Gap Statistics method
gap_stat <- clusGap(updated_data[,1:11], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
plot(gap_stat, main = "Gap Statistic Analysis")

# Silhouette method 
silhouette_scores <- lapply(2:10, function(k){ 
  model <- kmeans(updated_data[,1:11], centers = k) 
  ss <- silhouette(model$cluster, dist(updated_data[,1:11])) 
  mean(ss[,3]) 
}) 
plot(2:10, silhouette_scores, xlab = "Number of Clusters",  
     ylab = "Average Silhouette Score", type = "b", 
     main = "Silhouette Method") 

# c. k-means clustering
k<-3 
kmeans_output <- kmeans(updated_data[,1:11], centers = k)
print(kmeans_output)
print(kmeans_output$centers)
updated_data$cluster <- kmeans_output$cluster 
print(table(updated_data$cluster))

# Calculate BSS and WSS 
bss <- sum(kmeans_output$betweenss)
wss <- sum(kmeans_output$withinss) 
tss <- sum(kmeans_output$totss)
bss_tss_ratio <- bss / tss
print(c(BSS=bss, WSS=wss,TSS=tss, BSS_TSS_ratio=bss_tss_ratio))

# d. Silhouette plot( average width ) 
silplot <- silhouette(kmeans_output$cluster, dist(updated_data[,1:11])) 
print(mean(silplot[,3])) 
plot(silplot, main = "Silhouette Plot") 

#2nd Subtask

# e.PCA method
pca_white_wine <- prcomp(updated_data[,1:11], scale = TRUE) 
summary(pca_white_wine) 

plot(pca_white_wine, type = "l") 

# Calculate cumulative variance
cumulative_variance <- cumsum(pca_white_wine$sdev^2 / sum(pca_white_wine$sdev^2)) 


# cumulative variance plot 
plot(cumulative_variance, xlab = "Principal Component", ylab = "Cumulative Variance 
Explained", type = "b", main = "Cumulative Variance Plot") 
abline(h = 0.85, col = "red", lty = 2)  #horizontal line at 85%  

no_pc <- which(cumulative_variance >= 0.85)[1] 
cat("Minimum number of principal components to capture 85% variance:", no_pc, "\n") 

# final number of PCs 
final_no_pc <- no_pc   
cat("Final number of principal components retained:", final_no_pc, "\n") 

# PC scores data frame 
pc_scores <- as.data.frame(pca_white_wine$x) 

# new PCs dataset
updated_pca <- pc_scores[, 1:final_no_pc] 

# (f) Decide number of clusters using automated tools for PCA data 

# NBclust 
nb_clust_pca <- NbClust(updated_pca, min.nc = 2, max.nc = 10, method = "kmeans", index = "all") 
best_nb_clust_pca <- which.max(nb_clust_pca$Best.nc) 

# Elbow method 
fviz_nbclust(updated_pca, kmeans, method = "wss") + 
  labs(subtitle = "Elbow Method (PCA)") 

# Gap statistic 
gap_stat_pca <- suppressWarnings(clusGap(updated_pca, FUN = kmeans, nstart = 25, K.max = 10, B 
                                         = 50)) 
fviz_gap_stat(gap_stat_pca)

# Silhouette 
fviz_nbclust(updated_pca, kmeans, method='silhouette') +labs(title="Silhouette Method after PCA")

# g. Perform k-means clustering on the new dataset
#k_pca <- 2
#kmeans_output_pca <- kmeans(updated_pca, centers = k_pca)
#print(kmeans_output_pca)

k_pca <- 5
kmeans_output_pca <- kmeans(updated_pca, centers = k_pca)
print(kmeans_output_pca)
print(kmeans_output_pca$centers)
updated_pca$cluster <- kmeans_output_pca$cluster 
print(table(updated_pca$cluster))


# Calculate BSS,WSS,TSS and Ratio
bss_pca <- sum(kmeans_output_pca$betweenss)
wss_pca <- sum(kmeans_output_pca$tot.withinss)
tss_pca <- bss_pca + wss_pca
ratio_bss_tss_pca <- bss_pca / tss_pca
print(c(BSS_PCA=bss_pca, WSS_PCA=wss_pca,TSS_PCA=tss_pca, BSS_TSS_ratio_PCA=ratio_bss_tss_pca))

# h. Silhouette plot for PCA clustering 
pca_silplot <- silhouette(kmeans_output_pca$cluster, dist(updated_pca)) 
print(mean(pca_silplot[, 3]))  # Average silhouette width 
plot(pca_silplot, main = "Silhouette Plot (PCA)") 

# i.Calinski-Harabasz index
ch_index <- calinhara(updated_pca, kmeans_output_pca$cluster) 
print(ch_index)