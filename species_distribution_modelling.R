# Simple Species Distribution Modelling #
# Elias Bowman 
# 2025-04-03 
# For BIOL 488 Final Project

# Load required libraries
library(terra)      # For spatial data manipulation
library(leaflet)    # For interactive maps (not used here)
library(geodata)    # For downloading global bioclimatic data
library(predicts)   # For species distribution modeling (SDM)
library(data.table) # For efficient data handling
library(dplyr)      # For data manipulation
library(tidyverse)  # For advanced data wrangling

# Helper Functions ####

# Function to clean GBIF occurrence data
clean_gbif_data <- function(data, resolution = 1000) {
  data %>% 
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%  # Remove rows with missing lat/lon
    select(decimalLatitude, decimalLongitude, occurrenceStatus, coordinateUncertaintyInMeters) %>%  # Select relevant columns
    filter(coordinateUncertaintyInMeters < resolution, occurrenceStatus == "PRESENT") %>%  # Filter by uncertainty and presence status
    select(decimalLatitude, decimalLongitude) %>%  # Keep only lat/lon
    rename(latitude = decimalLatitude, longitude = decimalLongitude)  # Rename columns
}

# Function to determine geographic extent based on species coordinates
determine_extent <- function(coords) {
  max_lat <- ceiling(max(coords$latitude))  # Maximum latitude
  min_lat <- floor(min(coords$latitude))   # Minimum latitude
  max_lon <- ceiling(max(coords$longitude))  # Maximum longitude
  min_lon <- floor(min(coords$longitude))   # Minimum longitude
  ext(c(min_lon, max_lon, min_lat, max_lat)) * 1.25  # Add buffer to extent
}

# Function to generate background (absence) points from bioclim data
generate_background_points <- function(bioclim_data, size = 1000) {
  set.seed(100)  # Set seed for reproducibility
  spatSample(bioclim_data, size = size, values = FALSE, na.rm = TRUE, xy = TRUE) %>%  # Sample random points
    as.data.frame() %>%  # Convert to data frame
    rename(longitude = x, latitude = y) %>%  # Rename columns
    mutate(pa = 0)  # Assign absence value (pa = 0)
}

# Function to build a Generalized Linear Model (GLM) for species presence/absence
build_glm_model <- function(training_data) {
  glm(pa ~ ., data = training_data, family = binomial())  # Build logistic regression model
}

# Main Function ####

# Function to run species distribution modeling (SDM) using GBIF data
species_distribution_model <- function(gbif_data, uncertainty = 1000) {
  
  # Download global bioclimatic data (climate variables)
  bioclim_data <- worldclim_global(var = "bio", res = 2.5, path = "data/")
  
  # Clean GBIF data (filter by resolution and occurrence status)
  species_coords <- clean_gbif_data(gbif_data, resolution = uncertainty)
  
  # Determine geographic extent of species occurrence and crop bioclimatic data
  sample_extent <- determine_extent(species_coords)
  bioclim_data <- terra::crop(bioclim_data, sample_extent)  # Crop bioclim data based on extent
  
  # Generate background points (absence data) and combine with presence data
  presence <- species_coords %>% mutate(pa = 1)  # Mark species presence
  absence <- generate_background_points(bioclim_data)  # Generate background points (absence)
  all_points <- bind_rows(presence, absence)  # Combine presence and absence points
  
  # Extract climate data for all points
  bioclim_extract <- terra::extract(bioclim_data, all_points[, c("longitude", "latitude")], ID = FALSE)
  
  # Combine point coordinates with extracted climate data
  all_points_df <- as.data.frame(all_points)
  bioclim_extract_df <- as.data.frame(bioclim_extract)
  
  # Merge extracted climate data with species presence/absence data
  points_climate <- bind_cols(all_points_df, bioclim_extract_df) %>% dplyr::select(-longitude, -latitude)
  
  # Create train-test split for model validation (5-fold cross-validation)
  fold <- folds(points_climate, k = 5, by = points_climate$pa)
  training <- points_climate[fold != 1, ]  # Training data (all but fold 1)
  testing <- points_climate[fold == 1, ]   # Testing data (fold 1)
  
  # Build GLM model and predict species distribution
  glm_model <- build_glm_model(training)  # Build model on training data
  glm_predict <- predict(bioclim_data, glm_model, type = "response")  # Predict species distribution
  
  # Evaluate model performance using testing data
  glm_eval <- pa_evaluate(p = testing[testing$pa == 1, ], a = testing[testing$pa == 0, ], model = glm_model, type = "response")
  glm_threshold <- glm_eval@thresholds$max_spec_sens  # Determine optimal threshold for prediction
  
  # Download world map for visualization
  world_map <- world(resolution = 3, path = "data/")
  
  # Determine the extent of the species occurrence and crop world map
  extent <- determine_extent(species_coords)
  species_map <- crop(x = world_map, y = extent)
  
  # Return results as a list
  return(list(
    model = glm_model, 
    prediction = glm_predict, 
    evaluation = glm_eval, 
    threshold = glm_threshold, 
    region_map = species_map, 
    obs_coords = species_coords
  ))
}

# Example usage ####
# Load GBIF data (species occurrence data) from CSV
gbif_data <- as.data.frame(fread("data/castet_raw_download.csv"))

# Run species distribution model
species_dist <- species_distribution_model(gbif_data, uncertainty = 10000)

###########

# Plot SDM prediction and observed data points
plot(species_dist$prediction > species_dist$threshold, 
     legend = FALSE, 
     main = "Species Distribution Model", 
     col = c(NA, "#1A85FF"))  # Plot predicted suitable areas

# Overlay species region map
plot(species_dist$region_map, add = TRUE, border = "black")

# Plot observed species locations
points(
  species_dist$obs_coords$longitude, 
  species_dist$obs_coords$latitude, 
  col = "#D41159", 
  pch = 19, 
  cex = 0.20
)

# Plotting Functions ####

# Plot suitability with observed points overlaid
plot_obs_over_suitability <- function(species_dist) {
  raster_data <- species_dist$prediction
  species_coords <- species_dist$obs_coords
  basemap <- species_dist$region_map
  
  plot(raster_data, main = "Habitat Suitability Model", col = terrain.colors(100))  # Plot suitability
  plot(basemap, add = TRUE, border = "black")  # Overlay region map
  points(species_coords$longitude, species_coords$latitude, col = "purple", pch = 19, cex = 0.25)  # Plot observed points
}

plot_obs_over_suitability(species_dist)

# Plot SDM prediction (suitability) with threshold
plot_distribution_model <- function(species_dist) {
  raster_data <- species_dist$prediction
  threshold <- species_dist$threshold
  species_coords <- species_dist$obs_coords
  basemap <- species_dist$region_map
  
  suitability <- raster_data > threshold  # Apply threshold to prediction
  
  plot(suitability, 
       legend = FALSE, 
       main = "Species Distribution Model", 
       col = c(NA, "#fde725"))  # Plot suitability
  
  plot(basemap, add = TRUE, border = "black")  # Overlay region map
  
  points(
    species_coords$longitude, 
    species_coords$latitude, 
    col = "#482173", 
    pch = 19, 
    cex = 0.5
  )  # Plot observed points
}

# Save multiple plots with varying uncertainty levels
uncertainty_values <- c(10, 100, 1000, 10000, 100000)
for (uncertainty in uncertainty_values) {
  species_dist <- species_distribution_model(gbif_data, uncertainty = uncertainty)
  pdf(paste0("species_distribution_", uncertainty, ".pdf"), width = 8, height = 6, bg = "#f0f0f0")  # Light gray background
  plot_distribution_model(species_dist)
  dev.off()
}
