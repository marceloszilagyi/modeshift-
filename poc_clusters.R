# this script gets the US population and based on a given pop size
# return the location of the centroid of that number

library(tidyverse)
library(rvest)
library(janitor)
library(geosphere)
library(maps)
library(dbscan)

url <- "https://en.wikipedia.org/wiki/User:Michael_J/County_table#Table_of_United_States_counties"

# Read the HTML content from the page
page <- read_html(url)

# Find the table. Usually, it's the first table, but you might need to adjust the index
data <- page %>% 
  html_table(header = TRUE) %>%
  .[[1]]  # Change the index if the table is not the first one

# View the first few rows of the table
head(data)

data <- clean_names(data)

data <- data %>%
  mutate(across(-c(sort_1, state, county_2, county_seat_s_3, fips), ~parse_number(.))) 

data <- data %>% mutate(longitude = - longitude )


#excludes Alaska and PR
data_filtered <- data %>%
  filter(state != "AK", state != "PR",  state != "HI",)

#basic visualization
ggplot(data_filtered, aes(x = longitude, y = latitude, size = population_2010)) +
  geom_point(alpha = 0.3) +  # alpha for point transparency
  scale_size_continuous(range = c(1, 20),  # Adjust the min and max sizes as needed
                        breaks = quantile(data_filtered$population_2010, probs = seq(0, 1, 0.05), na.rm = TRUE)) +
  labs(title = "Scatter Plot of Latitude and Longitude by Population",
       x = "Longitude",
       y = "Latitude",
       size = "Population") +
  theme_minimal() 


# select a certain numbers of counties using the population density as reference

size_sample = 500
eps = 300 # radius
min_stops = 3

sampled_data <- data_filtered %>%
  slice_sample(n=  size_sample, weight_by = population_2010, replace = TRUE)

coords <- as.matrix(sampled_data[c("longitude", "latitude")])
dist_matrix <- distm(coords, fun = distHaversine)/1609.34 # Convert to miles

# Run DBSCAN - set eps to e miles and minPts as per your requirement
dbscan_result <- dbscan(dist_matrix, eps = eps, minPts = min_stops)

# Add cluster assignments to your dataframe
sampled_data$cluster <- dbscan_result$cluster

# increase the sample size and eps and plot number of opportunities. 
# for the n clusters defined, get the actual max distance between points 
# desired output - #sample size, # cases captured, # eps, #number_clusters, #max distance

# Function to compute maximum pairwise distance within a set of points
max_distance_within_cluster <- function(latitudes, longitudes) {
  # Create a matrix of coordinates
  coords <- cbind(longitudes, latitudes)
  
  # Calculate distance matrix
  dist_matrix <- distm(coords)/1609.34
  
  # Return the maximum distance
  max(dist_matrix)
}

sampled_data %>% filter(cluster != 0) %>%
  group_by(cluster) %>%
  summarise(max_distance = max_distance_within_cluster(latitude, longitude),
            count =n()) %>% 
  summarise(number_clusters = n(),
            max_distance = max(max_distance),
            count = sum(count))


####################


# Optionally, you can visualize the clusters if you have ggplot2
ggplot() +
  geom_point(data = sampled_data %>% filter(cluster != 0), aes(x = longitude, y = latitude, color = as.factor(cluster)), alpha = 0.7) +
  borders("state", colour = "gray", size = 0.5) + # Add US state borders
  theme_minimal() +
  labs(title = "Scatter Plot with US Contour", x = "Longitude", y = "Latitude") +
  coord_fixed(1.3) # Adjust aspect ratio if necessary

check <- sampled_data %>% filter(cluster == 1)
coords_check <- as.matrix(check[c("longitude", "latitude")])
dist_matrix_check <- distm(coords_check, fun = distHaversine)/1609.34



##################################

size_samples <- c(50,60,70,80,90,100)  
eps_values <- c(100,200,300,400)    
min_stops <- 3

# Function to run DBSCAN and calculate the required statistics
run_analysis <- function(size_sample, eps) {
  sampled_data <- data_filtered %>%
    slice_sample(n = size_sample, weight_by = population_2010, replace = TRUE)
  
  coords <- as.matrix(sampled_data[c("longitude", "latitude")])
  dist_matrix <- distm(coords, fun = distHaversine)/1609.34  # Convert to miles
  
  # Run DBSCAN
  dbscan_result <- dbscan(dist_matrix, eps = eps, minPts = min_stops)
  sampled_data$cluster <- dbscan_result$cluster
  
  # Calculate max distance and count
  cluster_summary <- sampled_data %>% 
    filter(cluster != 0) %>%
    group_by(cluster) %>%
    summarise(max_distance = max_distance_within_cluster(latitude, longitude),
              count = n()) %>% 
    summarise(number_clusters = n(),
              max_distance = max(max_distance),
              count = sum(count))
  
  cbind(size_sample, eps, cluster_summary)
  
  }
  
  

# Use expand.grid to create all combinations of size_samples and eps_values
combinations <- expand.grid(size_sample = size_samples, eps = eps_values)

# Use map_dfr to apply the function to each combination and row-bind the results
results <- pmap_dfr(combinations, ~run_analysis(..1, ..2))

results <- results %>% mutate(consol_rate = count/size_sample, shipments_cluster = count/number_clusters)



