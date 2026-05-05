library(ggplot2)

# loading csv data from github
data_url <- "https://raw.githubusercontent.com/jxxtino/tae-0126-guilherme-luana-matheus/refs/heads/main/data/02-processed/df_clean.txt"

# creating df and removing NA values
df <- read.csv(data_url, sep = ";")
df <- na.omit(df)

# features vector
features <- c("ndvi_mean", "ndbi_mean")

# scale the features (mean = 0 | sd = 1)
df_scaled <- df
df_scaled[, features] <- scale(df_scaled[,features])

# algorithm
km <- kmeans(df_scaled[,features], centers = 3, nstart = 25)

# cluster centers
round(km$centers, 2)

# clusters and predicted classes
table(cluster = km$cluster, outcome = df_scaled$ndvi_class)

# number of clusters
n_clusters <- 20

# vector of errors
wcss <- numeric(n_clusters)

# finding the best k
for (k in 1:n_clusters){
  km_it <- kmeans(df_scaled[,features], centers = k, nstart = 25, iter.max = 100)
  wcss[k] <- km_it$tot.withinss
}

# visualizing WCSS 
plot(
  1:n_clusters,
  wcss,
  type = "b",
  xlab = "N of Clusters",
  ylab = "WCSS",
  main = "Elbow Method"
)

# simple method for find the best k
d1 <- diff(wcss)
d2 <- diff(d1)
best_k <- which.max(abs(d2)) + 1

# algorithm with best_k
km_best_k <- kmeans(df_scaled[,features], centers = best_k, nstart = 25)

# best k clusters and predicted classes
table(cluster = km_best_k$cluster, outcome = df_scaled$ndvi_class)

# visualizing clusters
df$cluster <- as.factor(km_best_k$cluster)

ggplot(df, 
       aes(x = .data[[features[1]]], 
           y = .data[[features[2]]], 
           color = cluster)
       ) + geom_point(alpha = 0.5)

