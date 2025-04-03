# Load required libraries
library(terra)
library(leaflet)
library(geodata)
library(predicts)
library(data.table)
library(dplyr)
library(tidyverse)


# Helper Functions ####
clean_gbif_data <- function(data, resolution = 1000) {
  data %>% 
    filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
    dplyr::select(decimalLatitude, decimalLongitude, occurrenceStatus, coordinateUncertaintyInMeters) %>%
    filter(coordinateUncertaintyInMeters < resolution, occurrenceStatus == "PRESENT") %>%
    dplyr::select(decimalLatitude, decimalLongitude) %>%
    rename(latitude = decimalLatitude, longitude = decimalLongitude)
}

determine_extent <- function(coords) {
  max_lat <- ceiling(max(coords$latitude))
  min_lat <- floor(min(coords$latitude))
  max_lon <- ceiling(max(coords$longitude))
  min_lon <- floor(min(coords$longitude))
  ext(c(min_lon, max_lon, min_lat, max_lat)) * 1.25
}

generate_background_points <- function(bioclim_data, size = 1000) {
  set.seed(100)
  spatSample(bioclim_data, size = size, values = FALSE, na.rm = TRUE, xy = TRUE) %>%
    as.data.frame() %>%
    rename(longitude = x, latitude = y) %>%
    mutate(pa = 0)
}

build_glm_model <- function(training_data) {
  glm(pa ~ ., data = training_data, family = binomial())
}

# Main Function ####
species_distribution_model <- function(gbif_data, uncertainty = 1000) {
  # Download data for bioclimatic variables
  bioclim_data <- worldclim_global(var = "bio", res = 2.5, path = "data/")
  
  # Clean data
  species_coords <- clean_gbif_data(gbif_data, resolution = uncertainty)
  
  # Determine geographic extent and crop bioclim data
  sample_extent <- determine_extent(species_coords)
  bioclim_data <- terra::crop(bioclim_data, sample_extent)
  
  # Generate background points and prepare presence/absence data
  presence <- species_coords %>% mutate(pa = 1)
  absence <- generate_background_points(bioclim_data)
  all_points <- bind_rows(presence, absence)
  
  # Extract climate data
  bioclim_extract <- terra::extract(bioclim_data, all_points[, c("longitude", "latitude")], ID = FALSE)
  
  all_points_df <- as.data.frame(all_points)  # Convert to data.frame
  bioclim_extract_df <- as.data.frame(bioclim_extract)  # Convert extract result to data.frame
  
  points_climate <- bind_cols(all_points_df, bioclim_extract_df) %>% dplyr::select(-longitude, -latitude)
  
  # Create train-test split
  fold <- folds(points_climate, k = 5, by = points_climate$pa)
  training <- points_climate[fold != 1, ]
  testing <- points_climate[fold == 1, ]
  
  # Build GLM model and predict distribution
  glm_model <- build_glm_model(training)
  glm_predict <- predict(bioclim_data, glm_model, type = "response")
  
  glm_eval <- pa_evaluate(p = testing[testing$pa == 1, ], a = testing[testing$pa == 0, ], model = glm_model, type = "response")
  glm_threshold <- glm_eval@thresholds$max_spec_sens
  
  world_map <- world(resolution = 3,
                     path = "data/")
  extent <- determine_extent(species_coords)
  species_map <- crop(x = world_map, y = extent)
  
  return(list(model = glm_model, prediction = glm_predict, evaluation = glm_eval, threshold = glm_threshold, region_map = species_map, obs_coords = species_coords))
}


# Castet
gbif_data <- as.data.frame(fread("data/castet_raw_download.csv"))
species_dist <- species_distribution_model(gbif_data, uncertainty = 10000)

###########

plot(species_dist$prediction > species_dist$threshold, 
     #add = TRUE,
     legend = FALSE,
     main = "Species Distribution Model", 
     col = c(NA, "#1A85FF"))

plot(species_dist$region_map, add = TRUE, border = "black")

points(
  species_dist$obs_coords$longitude,
  species_dist$obs_coords$latitude,
  col = "#D41159",
  pch = 19,
  cex = 0.20
)



# species_dist$model
#plot(species_dist$prediction)
# species_dist$evaluation
# species_dist$threshold
# species_dist$region_map
# species_dist$obs_coords

# Plotting Functions ####

# Plot Suitability with Points Overlaid
plot_obs_over_suitability <- function(species_dist) {
  raster_data <- species_dist$prediction
  species_coords <- species_dist$obs_coords
  basemap <- species_dist$region_map
  
  plot(raster_data, main = "Habitat Suitability Model", col = terrain.colors(100))
  
  plot(basemap, add = TRUE, border = "black")
  
  points(species_coords$longitude, species_coords$latitude, col = "purple", pch = 19, cex = 0.25)
}

plot_obs_over_suitability(species_dist)


# Plot SDM
plot_distribution_model <- function(species_dist) {
  raster_data <- species_dist$prediction
  threshold <- species_dist$threshold
  species_coords <- species_dist$obs_coords
  basemap <- species_dist$region_map
  
  suitability <- raster_data > threshold
  

  plot(suitability, 
       #add = TRUE,
       legend = FALSE,
       main = "Species Distribution Model", 
       col = c(NA, "#fde725"))
  
  plot(basemap, add = TRUE, border = "black")
  
  points(
    species_coords$longitude,
    species_coords$latitude,
    col = "#482173",
    pch = 19,
    cex = 0.5
  )
}

species_dist <- species_distribution_model(gbif_data, uncertainty = 10)
pdf("species_distribution_10.pdf", width = 8, height = 6, bg = "#f0f0f0")  # Light gray background

plot_distribution_model(species_dist)

dev.off()

species_dist <- species_distribution_model(gbif_data, uncertainty = 100)
pdf("species_distribution_100.pdf", width = 8, height = 6, bg = "#f0f0f0")  # Light gray background

plot_distribution_model(species_dist)

dev.off()

species_dist <- species_distribution_model(gbif_data, uncertainty = 1000)
pdf("species_distribution_1000.pdf", width = 8, height = 6, bg = "#f0f0f0")  # Light gray background

plot_distribution_model(species_dist)

dev.off()

species_dist <- species_distribution_model(gbif_data, uncertainty = 10000)
pdf("species_distribution_10000.pdf", width = 8, height = 6, bg = "#f0f0f0")  # Light gray background

plot_distribution_model(species_dist)

dev.off()

species_dist <- species_distribution_model(gbif_data, uncertainty = 100000)
pdf("species_distribution_100000.pdf", width = 8, height = 6, bg = "#f0f0f0")  # Light gray background

plot_distribution_model(species_dist)

dev.off()


# 
# ####################
# 
# library(leaflet)
# library(leaflet.extras)
# library(terra)
# 
# # Convert raster prediction to a spatial format
# prediction_raster <- species_dist$prediction
# threshold_mask <- prediction_raster > species_dist$threshold
# 
# # Define a color palette for predictions
# pal <- colorNumeric(palette = "Blues", domain = values(threshold_mask), na.color = NA)
# 
# # Create leaflet map
# m <- leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery) %>%  # Use Esri satellite basemap
#   addRasterImage(threshold_mask, colors = pal, opacity = 0.5) %>%  # Overlay prediction raster
#   addPolygons(data = species_dist$region_map, color = "black", fill = FALSE) %>%  # Add region borders
#   addCircleMarkers(
#     lng = species_dist$obs_coords$longitude,
#     lat = species_dist$obs_coords$latitude,
#     color = "#D41159",
#     radius = 2,
#     stroke = FALSE,
#     fillOpacity = 0.7
#   ) %>%
#   addLegend(pal = pal, values = values(threshold_mask), title = "Species Presence Probability")  # Add legend
# 
# # Save map as HTML
# htmlwidgets::saveWidget(m, "species_map.html", selfcontained = TRUE)

