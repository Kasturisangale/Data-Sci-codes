# ================== Minimal Clustering Lab - K-Means & Hierarchical ==================
# Dataset: crime_safety_dataset.csv

# Install required packages if not present
required_pkgs <- c("tidyverse", "factoextra")
to_install <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install, repos='https://cloud.r-project.org')
lapply(required_pkgs, library, character.only = TRUE)

# ================== Load Dataset ==================
data <- read.csv("C:/Users/Kasturi/Downloads/crime_safety_dataset.csv", stringsAsFactors = FALSE)

# Clean dataset
data[data == ""] <- NA
data <- na.omit(data)

# ================== Select Numeric Features for Clustering ==================
# Using victim_age as numeric; you can add derived features if needed
num_data <- data %>% select(victim_age)

# ================== 1. K-MEANS CLUSTERING ==================
# Elbow Method to find optimal k
fviz_nbclust(num_data, kmeans, method = "wss") + 
  labs(title = "Elbow Method for Optimal k (Victim Age)")

# Apply K-means with k = 3 (adjust based on elbow plot)
set.seed(123)
k3 <- kmeans(num_data, centers = 3, nstart = 25)

# Visualize clusters
fviz_cluster(k3, data = num_data, geom = "point", ellipse.type = "norm") + 
  labs(title = "K-Means Clustering (k = 3)")

# Add cluster labels to dataset
data$Cluster_KMeans <- as.factor(k3$cluster)

# ================== 2. HIERARCHICAL CLUSTERING ==================
# Compute distance matrix
dist_matrix <- dist(num_data, method = "euclidean")

# Apply hierarchical clustering
hclust_model <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hclust_model, labels = FALSE, main = "Hierarchical Clustering Dendrogram (Victim Age)")
rect.hclust(hclust_model, k = 3, border = "red")  # Highlight 3 clusters

# Assign hierarchical clusters
data$Cluster_HC <- as.factor(cutree(hclust_model, k = 3))

# ================== 3. Cluster Characteristics ==================
cat("\n--- K-Means Cluster Characteristics ---\n")
print(aggregate(num_data, by = list(Cluster = data$Cluster_KMeans), mean))

cat("\n--- Hierarchical Cluster Characteristics ---\n")
print(aggregate(num_data, by = list(Cluster = data$Cluster_HC), mean))

# ================== 4. Experiment with Different k Values (Optional) ==================
for (k in 2:5) {
  set.seed(123)
  km <- kmeans(num_data, centers = k, nstart = 25)
  cat("\n--- K =", k, "---\n")
  cat("Total Within Sum of Squares:", round(km$tot.withinss, 2), "\n")
}
