# Species distribution test
# Elias Bowman
# elias.bowman@gmail.com
# 2024-03-11
#https://jcoliver.github.io/learn-r/011-species-distribution-models.html

# Import packages
library(tidyverse)

# GBIF Packages
# SDM Packages
library(terra)
library(geodata)
library(predicts)


# Import and download climate data
bioclim_data <- worldclim_global(var = "bio", # biolclimatic variables (19)
                                 res = 2.5, # 2.5 minute-degree resolution
                                 path = "data/")


# Import castet data downloaded from gbif
castet_data <- data.table::fread("data/castet_raw_download.csv")

# Clean castet data
castet_coords <- castet_data %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  select(decimalLatitude, decimalLongitude, occurrenceStatus, coordinateUncertaintyInMeters, coordinatePrecision, issue)

castet_coords <- castet_coords %>%
  filter(coordinateUncertaintyInMeters < 1000) %>%
  filter(occurrenceStatus == "PRESENT") %>%
  select(decimalLatitude, decimalLongitude) %>%
  rename(latitude = decimalLatitude, longitude = decimalLongitude)
  
# Save cleaned castet_coords
write.csv(castet_coords, file = "data/Cassiope-tetragona-GBIF.csv", row.names = FALSE)

# Reimport saved csv
obs_data <- read.csv(file = "data/Cassiope-tetragona-GBIF.csv")

# look at longitude and latitude cleaned
summary(obs_data)

# Generating Map of Observations ####
# Determine geographic extent of the data
max_lat <- ceiling(max(obs_data$latitude))
min_lat <- floor(min(obs_data$latitude))
max_lon <- ceiling(max(obs_data$longitude))
min_lon <- floor(min(obs_data$longitude))
# Store boundaries in a single extent object
geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))

# Download data with geodata's world function to use for our base map
world_map <- world(resolution = 3,
                   path = "data/")

# Crop the map to our area of interest
my_map <- crop(x = world_map, y = geographic_extent)

# Plot the base map
plot(my_map,
     axes = TRUE, 
     col = "grey95")

# Add the points for individual observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

# Creating Bioclimatic Raster ####
# Make an extent that is 10% larger
sample_extent <- geographic_extent * 1.10

# Crop bioclim data to desired extent
bioclim_data <- crop(x = bioclim_data, y = sample_extent)

# Plot the first of the bioclim variables to check on cropping
plot(bioclim_data[[1]])

# Pseudo Absence Approach ####
# Set the seed for the random-number generator to ensure results are similar
set.seed(100)

# Randomly sample points (same number as our observed points)
background <- spatSample(x = bioclim_data,
                         size = 1000,    # generate 1,000 pseudo-absence points
                         values = FALSE, # don't need values
                         na.rm = TRUE,   # don't sample from ocean
                         xy = TRUE)      # just need coordinates

# Look at first few rows of background
head(background)

# Plot the base map
plot(my_map,
     axes = TRUE, 
     col = "grey95")

# Add the background points
points(background,
       col = "grey30",
       pch = 1,
       cex = 0.75)

# Add the points for individual observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "olivedrab", 
       pch = 20, 
       cex = 0.75)

# Combining Datasets ####
## 1. Presence Data
# Pull out coordinate columns, x (longitude) first, then y (latitude) from species data
presence <- obs_data[, c("longitude", "latitude")]
# Add column indicating presence
presence$pa <- 1

## 2. Absence Data 
# Convert background data to a data frame
absence <- as.data.frame(background)
# Update column names so they match presence points
colnames(absence) <- c("longitude", "latitude")
# Add column indicating absence
absence$pa <- 0

# Join data into single data frame
all_points <- rbind(presence, absence)

# Reality check on data
head(all_points)

## 3.Climate Data
bioclim_extract <- extract( # takes geographic coordinates and raster data as input, and pulls out values in the raster data for each of the geographic coordinates.
  x = bioclim_data, # Raster dataset
  y = all_points[, c("longitude", "latitude")], # coordinates to pull from
  ID = FALSE) # No need for an ID column

# Add the point and climate datasets together
points_climate <- cbind(all_points, bioclim_extract)

# Identify columns that are latitude & longitude
drop_cols <- which(colnames(points_climate) %in% c("longitude", "latitude"))
drop_cols # print the values as a reality check

# Remove the geographic coordinates from the data frame
points_climate <- points_climate[, -drop_cols]


# Training and testing data ####
# Select data from training
# Keep points for validation

# Create vector indicating fold
fold <- folds( #evenly assign each point to a random group
  x = points_climate,
  k = 5,
  by = points_climate$pa)

table(fold) # keeping 1 asside for testing, 2-5 for training
testing <- points_climate[fold == 1, ]
training <- points_climate[fold != 1, ]

# Build Model using Training Data ####
glm_model <- glm(pa ~ ., # predict presence by everything but presence 
                 data = training, 
                 family = binomial())

# Predict Presence based on Bioclimate ####
# Get predicted values from the model
glm_predict <- predict(bioclim_data, glm_model, type = "response")

# Print predicted values
plot(glm_predict)

# Testing the Model ####
# Use testing data for model evaluation
glm_eval <- pa_evaluate(p = testing[testing$pa == 1, ],
                        a = testing[testing$pa == 0, ],
                        model = glm_model,
                        type = "response")

# Determine minimum threshold for "presence"
glm_threshold <- glm_eval@thresholds$max_spec_sens

# Plot base map
plot(my_map, 
     axes = TRUE, 
     col = "grey95")

# Only plot areas where probability of occurrence is greater than the threshold
plot(glm_predict > glm_threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = "olivedrab")

# And add those observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "black",
       pch = "+", 
       cex = 0.75)

# Redraw those country borders
plot(my_map, add = TRUE, border = "grey5")

# Comparison of two rasters returns only true or false
glm_predict > glm_threshold


# Plot base map
plot(my_map, 
     axes = TRUE, 
     col = "grey95")

# Only plot areas where probability of occurrence is greater than the threshold
plot(glm_predict > glm_threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab")) # <-- Update the values HERE

# And add those observations
points(x = obs_data$longitude, 
       y = obs_data$latitude, 
       col = "orange",
       pch = "+", 
       cex = 1)

# Redraw those country borders
plot(my_map, add = TRUE, border = "grey5")


# A final note on our approach: the map we have drawn presents a categorical classification of whether a particular point on the landscape will be suitable or not for the species of interest. This classification relies quite heavily on the value of the threshold (see glm_threshold and the documentation for pa_evaluate()) and the pseudo-absence points. Given that we used random sampling to generate those pseudo-absence points, there is potential for variation in the predicted range if you run this code more than once (try it! if you re-run the code from the point of creating the pseudo-absence points, you are almost guaranteed a different map.). There are a number of approaches to dealing with this variation, and the paper by Barbet-Massin et al. (2012) is a great resource. I’ll leave it as homework for you to determine which approach is most appropriate here!



