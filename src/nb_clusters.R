library(factoextra)
library(NbClust)

head(cluster_data)
head(cluster_vars)

cluster_data_nb <- cluster_data %>%
  select(one_of("country", cluster_vars))
cluster_data_nb <- as.data.frame(cluster_data_nb)
rownames(cluster_data_nb) <- cluster_data_nb$country
cluster_data_nb["country"] <- NULL

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")