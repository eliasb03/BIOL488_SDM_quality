
# Load necessary libraries
library(raster)
library(terra)
library(geodata)
library(predicts)
library(dplyr)
library(data.table)

rasters <- list(SDM_10, SDM_100, SDM_1000, SDM_10000)
# SDM_10$model
# SDM_10$evaluation
# SDM_10$threshold
# SDM_10$region_map
# SDM_10$obs_coords

SDM_10_predictions <- SDM_10$prediction > SDM_10$threshold
SDM_100_predictions <- SDM_100$prediction > SDM_100$threshold


par(mfrow=c(1,2)) # Setting up a side-by-side layout
plot(SDM_10_predictions, legend = FALSE, main = "Species Distribution Model", col = c(NA, "#1A85FF"))
plot(SDM_100_predictions, legend = FALSE, main = "Species Distribution Model", col = c(NA, "#1A85FF"))


# Agreement
# Calculate agreement between the two rasters
SDM_100_predictions_resampled <- resample(SDM_100_predictions, SDM_10_predictions, method = "bilinear")

SDM_10_values <- values(SDM_10_predictions)
SDM_100_values <- values(SDM_100_predictions_resampled)

# Calculate the proportion of agreement (1 means same, 0 means different)
agreement_vector <- (SDM_10_values == SDM_100_values) & !is.na(SDM_10_values) & !is.na(SDM_100_values)

# Compute the proportion of agreement
agreement_scalar <- sum(agreement_vector) / sum(!is.na(SDM_10_values) & !is.na(SDM_100_values))

# Print the scalar agreement value
print(agreement_scalar)

par(mfrow=c(1,1))
difference <- SDM_10_predictions - SDM_100_predictions_resampled

plot(difference, 
     main = "Difference Between SDM 10 and SDM 100 Predictions", 
     col = c("#1A85FF", "white", "#FF5733"),  # Blue for negative, white for zero, red for positive
     legend = TRUE)  # Remove default legend

# Add a custom legend
legend("topright", 
       legend = c("Negative Difference", "No Difference", "Positive Difference"), 
       fill = c("#1A85FF", "white", "#FF5733"), 
       title = "Difference", 
       cex = 0.8)  # Adjust the size of the legend text for readability
