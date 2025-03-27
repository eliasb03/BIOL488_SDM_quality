# Load necessary libraries
library(terra)          # For spatial data manipulation
library(geodata)        # For accessing global climate data
library(predicts)       # For species distribution models
library(dplyr)          # For data manipulation
library(data.table)     # For fast data processing
#library(ggplot2)        # For plotting

# Helper Functions ####

# Function to clean and filter GBIF data
clean_gbif_data <- function(data, resolution) {
  data %>%
    # Filter out missing coordinates and ensure occurrence is "PRESENT"
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
    dplyr::select(decimalLatitude, decimalLongitude, occurrenceStatus, coordinateUncertaintyInMeters) %>%
    # Filter by coordinate uncertainty and occurrence status
    filter(coordinateUncertaintyInMeters < resolution, occurrenceStatus == "PRESENT") %>%
    # Rename the latitude and longitude columns
    dplyr::select(decimalLatitude, decimalLongitude) %>%
    rename(latitude = decimalLatitude, longitude = decimalLongitude)
}

# Function to clean lowercase and underscore names
capitalize_species <- function(species_name) {
  # Replace underscores with spaces and capitalize the first letter of each word
  species_name <- gsub("_", " ", species_name)  # Replace underscores with spaces
  species_name <- tools::toTitleCase(species_name)  # Capitalize first letter of each word
  return(species_name)
}

# Function to determine the extent (bounding box) of the species' occurrence
determine_extent <- function(coords) {
  max_lat <- ceiling(max(coords$latitude))  # Maximum latitude
  min_lat <- floor(min(coords$latitude))    # Minimum latitude
  max_lon <- ceiling(max(coords$longitude)) # Maximum longitude
  min_lon <- floor(min(coords$longitude))   # Minimum longitude
  
  # Expand the extent by 25%
  ext(c(min_lon, max_lon, min_lat, max_lat)) * 1.25
}

# Function to generate random background points for absence data
generate_background_points <- function(bioclim_data, size = 1000) {
  set.seed(100)
  spatSample(bioclim_data, size = size, values = FALSE, na.rm = TRUE, xy = TRUE) %>%
    as.data.frame() %>%
    rename(longitude = x, latitude = y) %>%
    mutate(pa = 0)  # Background points labeled as absence (pa = 0)
}

# Function to build a Generalized Linear Model (GLM)
build_glm_model <- function(training_data) {
  glm(pa ~ ., data = training_data, family = binomial())  # Binomial logistic regression
}

# Main function for running the species distribution model (SDM)
species_distribution_model <- function(gbif_data, uncertainty) {
  # Load worldclim global climate data (bioclim variables)
  bioclim_data <- worldclim_global(var = "bio", res = 2.5, path = "data/")
  
  # Clean GBIF data and extract species coordinates
  species_coords <- clean_gbif_data(gbif_data, resolution = uncertainty)
  
  # Determine the spatial extent of the species' occurrence
  sample_extent <- determine_extent(species_coords)
  
  # Crop the bioclim data to the species occurrence extent
  bioclim_data <- terra::crop(bioclim_data, sample_extent)
  
  # Create a data frame with presence (1) and absence (0) points
  presence <- species_coords %>% mutate(pa = 1)  # Presence points labeled as 1
  absence <- generate_background_points(bioclim_data)  # Absence points
  
  # Combine presence and absence data
  all_points <- bind_rows(presence, absence)
  
  # Extract bioclimatic variables at the occurrence points
  bioclim_extract <- terra::extract(bioclim_data, all_points[, c("longitude", "latitude")], ID = FALSE)
  
  # Bind extracted climate data with occurrence data
  points_climate <- bind_cols(all_points, bioclim_extract) %>% dplyr::select(-longitude, -latitude)
  
  # Create 5-fold cross-validation
  fold <- folds(points_climate, k = 5, by = points_climate$pa)
  
  # Split the data into training and testing sets
  training <- points_climate[fold != 1, ]
  testing <- points_climate[fold == 1, ]
  
  # Build the GLM model
  glm_model <- build_glm_model(training)
  
  # Predict species presence using the model
  glm_predict <- predict(bioclim_data, glm_model, type = "response")
  
  # Evaluate model performance (maximizing sensitivity and specificity)
  glm_eval <- pa_evaluate(p = testing[testing$pa == 1, ], a = testing[testing$pa == 0, ], model = glm_model, type = "response")
  
  # Set the optimal threshold based on evaluation
  glm_threshold <- glm_eval@thresholds$max_spec_sens
  
  # Load a world map as a base map for visualization
  world_map <- world(resolution = 3, path = "data/")
  
  # Crop the world map to the extent of the species occurrence
  extent <- determine_extent(species_coords)
  species_map <- crop(x = world_map, y = extent)
  
  # Return the model, prediction, evaluation results, threshold, and map
  return(list(model = glm_model, prediction = glm_predict, evaluation = glm_eval, threshold = glm_threshold, region_map = species_map, obs_coords = species_coords))
}

# Function to plot and save the species distribution model (SDM) results
plot_distribution_model <- function(species_dist, output_file, species_name) {
  raster_data <- species_dist$prediction  # Model prediction
  threshold <- species_dist$threshold      # Threshold for species presence
  species_coords <- species_dist$obs_coords  # Species occurrence points
  basemap <- species_dist$region_map      # Base map
  
  # Create a suitability map where prediction > threshold
  suitability <- raster_data > threshold
  
  nice_name <- capitalize_species(species_name)
  
  # Save the plot as a PDF
  pdf(output_file)
  plot(suitability, legend = FALSE, main = paste0(nice_name, " Species Distribution Model"), col = c(NA, "#1A85FF"))
  plot(basemap, add = TRUE, border = "black")
  
  # Plot the species occurrence points on top
  points(species_coords$longitude, species_coords$latitude, col = "#D41159", pch = 19, cex = 0.20)
  
  # Close the PDF device
  dev.off()
  
  # Save the plot as a high-resolution PNG
  png_filename <- sub("\\.pdf$", ".png", output_file)  # Replace .pdf with .png
  png(png_filename, width = 2000, height = 1500, res = 100, bg = "transparent")  # Higher resolution
  plot(suitability, legend = FALSE, main = paste0(nice_name, " Species Distribution Model"), col = c(NA, "#1A85FF"))
  plot(basemap, add = TRUE, border = "black")
  points(species_coords$longitude, species_coords$latitude, col = "#D41159", pch = 19, cex = 0.20)
  dev.off()
  
  message("Species distribution model plot saved as ", output_file, " and ", png_filename)
}

# Function to run a series of SDMs for different uncertainty levels and keep the models in the R environment
run_sdm_series <- function(gbif_data, species_name) {
  # Define a vector of uncertainty levels (in meters)
  uncertainty_levels <- c(10, 100, 1000, 10000)
  
  # Loop through each uncertainty level
  for (uncertainty in uncertainty_levels) {
    message("Running SDM for ", species_name, " with uncertainty ", uncertainty, "m")
    
    # Run the SDM for the current uncertainty level
    species_dist <- species_distribution_model(gbif_data, uncertainty)
    
    # Store the SDM model result in the environment with a dynamic name
    model_name <- paste0("SDM_", uncertainty)
    assign(model_name, species_dist, envir = .GlobalEnv)
    
    # Define file path for the plot and save the plot as PDF
    pdf_file <- paste0(species_name, "_SDM_", uncertainty, "m.pdf")
    plot_distribution_model(species_dist, pdf_file, species_name)
    
  }
}

# Example Usage ####

# Load GBIF data (example file)
gbif_data <- fread("data/castet_raw_download.csv")

# Specify the species name for analysis
species_name <- "cassiope_tetragona"

# Run the SDM series for the specified species
run_sdm_series(gbif_data, species_name)
