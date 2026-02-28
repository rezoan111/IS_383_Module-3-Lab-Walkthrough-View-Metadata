# Chapter 6 - Module 3 Lab Walkthrough
# Problem 18 (Distance to centroid - Wine clusters)
# Problem 20 (k-means vs Single Linkage - 2 clusters)
#
# Name: Rezoan Ishteaque Hossain

# -----------------------------
# Setup (install/load packages)
# -----------------------------

# run this one time (installs Chapter 6 packages)
# source("Chapter 6 Libraries.R")

library(tidyverse)
library(readxl)
library(cluster)
library(factoextra)
library(lubridate)

# -----------------------------
# Problem 18: Distance to centroid
# -----------------------------

centroids <- tibble::tibble(
  Characteristic = c("Alcohol","Malic Acid","Ash","Alkalinity","Magnesium","Phenols","Flavonoids",
                     "Nonflavonoids","Proanthocyanidins","Color Intensity","Hue","Dilution","Proline"),
  Cluster1 = c(0.819, -0.329, 0.248, -0.677, 0.643, 0.825, 0.896,
               -0.595, 0.619, 0.135, 0.497, 0.744, 1.117),
  Cluster2 = c(0.164, 0.869, 0.186, 0.523, -0.075, 0.977, -1.212,
               0.724, -0.778, 0.939, -1.162, -1.289, -0.406),
  Cluster3 = c(-0.937, -0.368, -0.393, 0.249, -0.573, -0.034, 0.083,
               0.009, 0.010, -0.881, 0.437, 0.295, -0.776)
)

new_wine <- tibble::tibble(
  Characteristic = c("Alcohol","Malic Acid","Ash","Alkalinity","Magnesium","Phenols","Flavonoids",
                     "Nonflavonoids","Proanthocyanidins","Color Intensity","Hue","Dilution","Proline"),
  value = c(-1.023, -0.480, 0.049, 0.600, -1.242, 1.094, 0.001,
            0.548, -0.229, -0.797, 0.711, -0.425, 0.010)
)

distances <- centroids %>%
  dplyr::left_join(new_wine, by = "Characteristic") %>%
  dplyr::summarise(
    Cluster1_Distance = sqrt(sum((Cluster1 - value)^2)),
    Cluster2_Distance = sqrt(sum((Cluster2 - value)^2)),
    Cluster3_Distance = sqrt(sum((Cluster3 - value)^2))
  )

distances

closest_cluster <- distances %>%
  tidyr::pivot_longer(everything(), names_to = "Cluster", values_to = "Distance") %>%
  dplyr::arrange(Distance)

closest_cluster


# -----------------------------
# Problem 20: k-means vs single linkage (2 clusters)
# -----------------------------

employees_raw <- readr::read_csv("llemployees_r.csv", show_col_types = FALSE)

# Extract Day-Month-Year-Performance-Code from EmpID
emp <- employees_raw %>%
  tidyr::separate(
    EmpID,
    into = c("Day", "Month", "Year", "Perf", "Code"),
    sep = "-",
    convert = TRUE
  ) %>%
  dplyr::mutate(
    DOB = lubridate::make_date(Year, Month, Day),
    Age = floor(lubridate::time_length(lubridate::interval(DOB, Sys.Date()), "years")),
    Performance = as.integer(Perf)
  ) %>%
  dplyr::filter(dplyr::between(Age, 18, 70)) %>%   # remove invalid ages
  dplyr::select(Age, Performance)

# scatter (cleaned)
ggplot(emp, aes(x = Age, y = Performance)) +
  geom_point() +
  labs(title = "Employees: Age vs Performance (Cleaned)",
       x = "Age",
       y = "Performance Rating")

# scale the data (important for clustering)
X <- scale(emp)

# ---- (a) K-means (k = 2) ----
set.seed(1)
km2 <- kmeans(X, centers = 2, nstart = 25)

emp_km <- emp %>% mutate(Cluster = factor(km2$cluster))

p_km <- ggplot(emp_km, aes(x = Age, y = Performance, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "k-means Clustering (k = 2)",
       x = "Age",
       y = "Performance Rating")

p_km
ggsave("Q20_kmeans.png", plot = p_km, width = 7, height = 5, dpi = 200)

emp_km %>%
  group_by(Cluster) %>%
  summarise(n = n(),
            avg_age = mean(Age),
            avg_perf = mean(Performance))

# ---- (b) Single linkage hierarchical (2 clusters) ----
d <- dist(X, method = "euclidean")
hc_single <- hclust(d, method = "single")
single2 <- cutree(hc_single, k = 2)

emp_single <- emp %>% mutate(Cluster = factor(single2))

p_single <- ggplot(emp_single, aes(x = Age, y = Performance, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "Single Linkage Clustering (2 clusters)",
       x = "Age",
       y = "Performance Rating")

p_single
ggsave("Q20_singlelinkage.png", plot = p_single, width = 7, height = 5, dpi = 200)

emp_single %>%
  group_by(Cluster) %>%
  summarise(n = n(),
            avg_age = mean(Age),
            avg_perf = mean(Performance))
