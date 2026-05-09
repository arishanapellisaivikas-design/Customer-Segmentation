# Load libraries
library(ggplot2)
library(dplyr)

# 1. Load Dataset
data <- read.csv("C:/Users/arish/Downloads/Project 3 Customer Segmentation/Mall_Customers_dataset.csv")

head(data)
colnames(data)
str(data)

# 2. Data Cleaning

# Remove missing values
data <- na.omit(data)

# Remove duplicates
data <- unique(data)

# Convert numeric columns
data$Age <- as.numeric(data$Age)
data$Annual.Income..k.. <- as.numeric(data$Annual.Income..k..)
data$Spending.Score..1.100. <- as.numeric(data$Spending.Score..1.100.)

# 3. Select Features for Clustering

customer_data <- data[, c("Annual.Income..k..",
                          "Spending.Score..1.100.")]

# 4. Apply K-Means Clustering

set.seed(123)

kmeans_result <- kmeans(customer_data,
                        centers = 3)

# Add Cluster Labels
data$Cluster <- as.factor(kmeans_result$cluster)

# 5. Cluster Summary

cluster_summary <- data %>%
  group_by(Cluster) %>%
  summarise(
    Average_Income = mean(Annual.Income..k..),
    Average_Spending = mean(Spending.Score..1.100.),
    Total_Customers = n()
  )

print(cluster_summary)

# 6. Visualization

# Customer Segmentation Scatter Plot
plot1 <- ggplot(data,
                aes(x = Annual.Income..k..,
                    y = Spending.Score..1.100.,
                    color = Cluster)) +
  
  geom_point(size = 3) +
  
  labs(
    title = "Customer Segmentation using K-Means Clustering",
    x = "Annual Income (k$)",
    y = "Spending Score"
  ) +
  
  theme_minimal()

# Age Distribution
plot2 <- ggplot(data,
                aes(x = Age,
                    fill = Cluster)) +
  
  geom_histogram(binwidth = 5,
                 alpha = 0.7,
                 position = "identity") +
  
  labs(
    title = "Age Distribution by Cluster",
    x = "Age",
    y = "Count"
  ) +
  
  theme_minimal()

# Gender-wise Cluster Distribution
plot3 <- ggplot(data,
                aes(x = Gender,
                    fill = Cluster)) +
  
  geom_bar(position = "dodge") +
  
  labs(
    title = "Gender-wise Customer Clusters",
    x = "Gender",
    y = "Number of Customers"
  ) +
  
  theme_minimal()

# 7. Show Plots
print(plot1)
print(plot2)
print(plot3)
