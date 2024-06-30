# Load necessary libraries
library(sf)          # For working with spatial data and shapefiles
library(ggplot2)     # For plotting with ggplot
library(spatstat)    # For spatial point pattern analysis
library(viridis)     # For color scales in plots
library(ggspatial)
library(viridis)

# SPATIAL DISTRIBUTION OF LEARNING SPACES POI IN LIPA
# Read Lipa City Shapefile and plot polygon
sf <- st_read("CityOfLipa_shape.shp")   # Read Lipa City shapefile
lipa_sf <- st_geometry(sf)              # Extract geometry
lipa_sf <- st_transform(lipa_sf, crs = 4326)  # Transform to EPSG:4326
lipa_sf_poly <- ggplot() + 
  geom_sf(data = lipa_sf, color = "red", fill = "#ffded6") +  # Plot Lipa City polygon
  ggtitle("Polygon of Lipa City") + 
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering)
lipa_sf_poly  # Display plot

# Read CSV files containing coordinates
learning_spaces <- read.csv("learningspace.csv")
schools <- read.csv("schools.csv")

# Convert CSV data to sf objects
learning_spaces_sf <- st_as_sf(learning_spaces, coords = c("Longitude", "Latitude"), crs = 4326)
schools_sf <- st_as_sf(schools, coords = c("Longitude", "Latitude"), crs = 4326)

# Plot coordinates on top of Lipa City polygon
lipa_sf_plot <- ggplot() + 
  geom_sf(data = lipa_sf, color = "red", fill = "#ffded6") +
  geom_sf(data = learning_spaces_sf, aes(geometry = geometry), color = "darkred", size = 2) +
  geom_sf(data = schools_sf, aes(geometry = geometry), color = "#33a02c", size = 2) +
  ggtitle("Spatial Distribution of Schools and Learning Spaces in Lipa City") + 
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  xlab("Longitude") +   
  ylab("Latitude")
lipa_sf_plot  # Display plot

# Project Lipa City to a suitable projected coordinate system (UTM Zone 51N)
lipa_sf_proj <- st_transform(lipa_sf, crs = 32651)

# Transform learning spaces to the same projected CRS
learning_spaces_sf_proj <- st_transform(learning_spaces_sf, crs = 32651)

# Convert to spatstat ppp object
window <- as.owin(lipa_sf_proj)  # Define spatial window
points_ppp <- as.ppp(st_coordinates(learning_spaces_sf_proj), window)  # Convert to spatstat ppp

# KERNEL DENSITY ESTIMATION(KDE) OF LEARNING SPACES POI ####
kde <- density.ppp(points_ppp, sigma = bw.diggle(points_ppp), dimyx = c(1000, 1000), edge = TRUE)  # KDE calculation
View(kde)  # Display KDE object in RStudio's data viewer
plot(kde)

# Convert KDE results to data frame
kde_df <- as.data.frame(as.im(kde))  # Convert im object to data frame

# Rename columns for convenience
colnames(kde_df) <- c("x", "y", "density")

# Plot KDE using ggplot2
kde_learning_100 <- ggplot(kde_df, aes(x = x, y = y, fill = density)) +
  geom_raster(interpolate = TRUE) +  # Use raster for continuous density map
  scale_fill_viridis() +             # Use viridis color scale
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  labs(title = "Kernel Density Estimate of Learning Spaces POI (bandwidth = 100)",
       x = "Longitude",
       y = "Latitude",
       fill = "Density") +
  theme_minimal()
kde_learning_100 

# BUILDING MODELS ####
data <- read.csv("final.csv")  # Read CSV data

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)  # Convert to sf object

lipa_sf_proj <- st_transform(lipa_sf, crs = 32651)  # Project Lipa City to UTM Zone 51N

data_sf_proj <- st_transform(data_sf, crs = 32651)  # Transform data to projected CRS

window <- as.owin(lipa_sf_proj)  # Define spatial window
learningspaces.ppp <- as.ppp(st_coordinates(data_sf_proj), window)  # Convert to spatstat ppp

# Transform NTL to spatial image
ntl <- cbind(data$longitude, data$latitude, data$pred_ntl_antilog)
colnames(ntl) <- c("x", "y", "ntl")
ntl <- as.data.frame(ntl)

ntl_sf <- st_as_sf(ntl, coords = c("x", "y"), crs = 4326)
ntl_sf_proj <- st_transform(ntl_sf, crs = 32651)

ntl_ppp <- as.ppp(ntl_sf_proj, W = window)
NTL <- Smooth(ntl_ppp)

# Transform NDVI to spatial image
ndvi <- cbind(data$longitude, data$latitude, data$pred_ndvi)
colnames(ndvi) <- c("x", "y", "ndvi")
ndvi <- as.data.frame(ndvi)

ndvi_sf <- st_as_sf(ndvi, coords = c("x", "y"), crs = 4326)
ndvi_sf_proj <- st_transform(ndvi_sf, crs = 32651)

ndvi_ppp <- as.ppp(ndvi_sf_proj, W = window)
NDVI <- Smooth(ndvi_ppp)

# Transform DIST to spatial image
dist <- cbind(data$longitude, data$latitude, data$dist_nearest_school)
colnames(dist) <- c("x", "y", "dist")
dist <- as.data.frame(dist)

dist_sf <- st_as_sf(dist, coords = c("x", "y"), crs = 4326)
dist_sf_proj <- st_transform(dist_sf, crs = 32651)

dist_ppp <- as.ppp(dist_sf_proj, W = window)
DistNS <- Smooth(dist_ppp)

#MODELING THE INTENSITY OF LEARNING SPACES ####
model1 <- kppm(learningspaces.ppp ~ NTL + NDVI + distNS, "LGCP")  # Model 1
summary(model1)
plot(predict(model1))
plot(simulate(model1, n = 1))
confint(model1)
AIC(model1)
plot(simulate(model1, n = 4))

model2 <- kppm(learningspaces.ppp ~ NTL + distNS, "LGCP")  # Model 2

model3 <- kppm(learningspaces.ppp ~ NDVI, "LGCP")  # Model 3

model4 <- ppm(learningspaces.ppp ~ NTL + NDVI + DistNS)  # Model 4
summary(model4)
predicted_model4 <- predict(model4)

predicted_model_df <- as.data.frame.im(predicted_model4)

ggplot(predicted_model_df, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "turbo", name = "Intensity") +  
  labs(
    title = "Predicted Intensity of Learning Spaces",
    x = "Longitude",  
    y = "Latitude"    
  ) +
  theme_minimal()  



# Predicting intensity in Region of Interest (ROI)
# Define ROIs in Lipa
roi1 <- owin(xrange = c(303000, 304000), yrange = c(1546000, 1548000))
roi2 <- owin(xrange = c(302500, 303500), yrange = c(1549000, 1550000))

# Highlight ROIs over Spatial Distribution of Learning Spaces
plot(lipa_sf_proj)
plot(learning_spaces_sf_proj, col = "black", add = TRUE)
plot(roi1, add = TRUE, border = "gold", lwd = 2)
plot(roi2, add = TRUE, border = "red", lwd = 2)

# Highlight ROIs over Predicted Intensity of Learning Spaces
plot(predicted_intensity, axes = TRUE)  
plot(roi1, add = TRUE, border = "gold", lwd = 2)
plot(roi2, add = TRUE, border = "red", lwd = 2)

# Predictions
predicted_intensity_roi1 <- predict(modelppm, window = roi1) 
predicted_count_roi1 <- predict(modelppm, type = "count", window = roi1)

predicted_intensity_roi2 <- predict(modelppm, window = roi2)  
predicted_count_roi2 <- predict(modelppm, type = "count", window = roi2)

# Plot predicted intensity
plot(predicted_intensity_roi1, axes = TRUE)
plot(predicted_intensity_roi2, axes = TRUE)




