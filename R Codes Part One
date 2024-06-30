#NOTE: the package "rgdal" is no longer supported
install.packages("raster")
install.packages("sf",type="source", args=c('--no-multiarch','--no-test-load'))
install.packages("sf")
install.packages("geodata")
install.packages("terra")
options("install.lock"=FALSE)
install.packages("stars", args=c('--no-multiarch','--no-test-load'))
install.packages("tmaptools")
install.packages("patchwork")
install.packages("gridExtra")
install.packages("ggspatial")
install.packages("automap")
install.packages("ggspatial")
install.packages("openxlsx")
install.packages("akima")
library(akima)
library(openxlsx)
library(tmaptools)
library(stars)
library(sf)
library(raster)
library(sp)
library(geodata)
library(terra)
library(ggplot2)
library(spatstat)
library(dplyr)
library(viridis)
library(devtools)
library(stringr)
library(patchwork)
library(gridExtra)
library(MASS)
library(KernSmooth)
library(maps)
library(ggspatial)
library(gstat)
library(automap)
library(DescTools)
library(ggspatial)

find_rtools()
writeLines('PATH="${RTOOLS44_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

#####SECTION 1 - EXPLORATION --------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##GRATICULE MAPS OF LOCATIONS --------------------------------------------------
provinces_sf <- st_read("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/Nighttime Lights/PH_Adm2_ProvDists.shp.shp")
ggplot() +
  geom_sf(data = provinces_sf, aes(fill = factor(adm2_en))) +
  scale_fill_manual(values = c("Batangas" = "blue",
                               "other" = "gray"))
muni_sf <- st_read("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/Nighttime Lights/Municities.shp.shp")
muni_sf <- st_set_crs(muni_sf, 4326)
crs(muni_sf)
ggplot() + 
  geom_sf(data = muni_sf, aes(color = factor(name), fill = factor(name))) +
  scale_color_manual(values = c("Calatagan" = "blue",
                               "Lian" = "blue",
                               "Bauan" = "blue",
                               "Nasugbu" = "blue",
                               "Tuy" = "blue",
                               "Balayan" = "blue",
                               "City of Calaca" = "blue",
                               "Lemery" = "blue",
                               "Agoncillo" = "blue",
                               "San Nicolas" = "blue",
                               "Taal" = "blue",
                               "Santa Teresita" = "blue",
                               "San Luis" = "blue",
                               "Mabini" = "blue",
                               "Batangas City" = "blue",
                               "Lobo" = "blue",
                               "Tingloy" = "blue",
                               "Taysan" = "blue",
                               "Rosario" = "blue",
                               "San Juan" = "blue",
                               "City of Lipa" = "red",
                               "Mataasnakahoy" = "blue",
                               "Balete" = "blue",
                               "Malvar" = "blue",
                               "City of Tanauan" = "blue",
                               "Talisay" = "blue",
                               "Laurel" = "blue",
                               "Padre Garcia" = "blue",
                               "Ibaan" = "blue",
                               "San Jose" = "blue",
                               "San Pascual" = "blue",
                               "Cuenca" = "blue",
                               "Alitagtag" = "blue",
                               "City of Sto. Tomas" = "blue")) +
  scale_fill_manual(values = c("Calatagan" = "#d6e7ff",
                               "Bauan" = "#d6e7ff",
                               "Lian" = "#d6e7ff",
                               "Nasugbu" = "#d6e7ff",
                               "Tuy" = "#d6e7ff",
                               "Balayan" = "#d6e7ff",
                               "City of Calaca" = "#d6e7ff",
                               "Lemery" = "#d6e7ff",
                               "Agoncillo" = "#d6e7ff",
                               "San Nicolas" = "#d6e7ff",
                               "Taal" = "#d6e7ff",
                               "Santa Teresita" = "#d6e7ff",
                               "San Luis" = "#d6e7ff",
                               "Mabini" = "#d6e7ff",
                               "Batangas City" = "#d6e7ff",
                               "Lobo" = "#d6e7ff",
                               "Tingloy" = "#d6e7ff",
                               "Taysan" = "#d6e7ff",
                               "Rosario" = "#d6e7ff",
                               "San Juan" = "#d6e7ff",
                               "City of Lipa" = "#ffded6",
                               "Mataasnakahoy" = "#d6e7ff",
                               "Balete" = "#d6e7ff",
                               "Malvar" = "#d6e7ff",
                               "City of Tanauan" = "#d6e7ff",
                               "Talisay" = "#d6e7ff",
                               "Laurel" = "#d6e7ff",
                               "Padre Garcia" = "#d6e7ff",
                               "Ibaan" = "#d6e7ff",
                               "San Jose" = "#d6e7ff",
                               "San Pascual" = "#d6e7ff",
                               "Cuenca" = "#d6e7ff",
                               "Alitagtag" = "#d6e7ff",
                               "City of Sto. Tomas" = "#d6e7ff")) +
  coord_sf(xlim = c(120.447644, 121.534506),
         ylim = c(13.490837, 14.300622)) +
  theme(legend.position = "none")

#Read the Lipa City Shape File
sf <- st_read("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/Nighttime Lights/CityOfLipa_shape.shp")
lipa_sf <- st_geometry(sf)
lipa_sf <- st_transform(lipa_sf, crs = 4326)
ggplot() + 
  geom_sf(data = lipa_sf, color = "red", fill = "#ffded6") +
  ggtitle("Polygon of Lipa City") + 
  annotation_scale(location = "bl", width_hint = 0.2, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) 

#Read the datasets
ntl <- read.csv("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/Nighttime Lights/NTL_pixels.csv")
vi <- read.csv("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/Vegetation Index/vegetation indices.csv")
annual <- read.csv("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/Nighttime Lights/annual/ntl_annual.csv")
colnames(annual) <- c("longitude","latidude","2014","2015","2016","2017","2018","2019","2020","2021","2022")
View(annual)

#Convert the data frame of points to an sf object
ntl_sf <- st_as_sf(ntl, coords = c("longitude", "latitude"), crs = 4326)
vi_sf <- st_as_sf(vi, coords = c("longitude", "latitude"), crs = 4326)
#CRS 432 corresponds to WGS 84 coordinate system, which the VIIRS uses

#PIXEL POINT PLOTS -------------------------------------------------------------
ntlpoints <- ggplot() +
  geom_sf(data = lipa_sf) +
  geom_sf(data = ntl_sf, color = "#597cde") +annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  ggtitle("NTL Points over Lipa City (925 points)") 
ndvipoints <- ggplot() +
  geom_sf(data = lipa_sf) +
  geom_sf(data = vi_sf, color = "#7ade59") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  ggtitle("NDVI Points over Lipa City (894 points)") 
grid.arrange(ntlpoints,ndvipoints,ncol=2)

plot(ndvipoints)
##RASTER PLOT FOR JAN 2014 NTL -------------------------------------------------
ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = avg_rad)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  geom_sf(data = lipa_sf, fill = NA, color = "black") + 
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  ggtitle("Lipa City Nighttime Lights for January 2024")
#NOTES: the raggedness of values particularly near the boundary is caused by 
#the spatial resolution innate to the satellite image, becoming a limiting factor
#to the visualization and later analyses (https://stackoverflow.com/questions/64380427/increase-pixel-resolution-of-plot)

mean(ntl$avg_rad)

##HISTOGRAM OF ANNUAL NTL ------------------------------------------------------
years <- as.character(2014:2022)
averages <- colMeans(annual[, years])
average_df <- data.frame(
  Year = years,
  Average = averages
)
average_df
ggplot(average_df, aes(x = Year, y = Average)) +
  geom_bar(stat = "identity", fill = "darkgray") +
  geom_point(color = "darkblue", size = 3) +
  geom_line(group = 1, color = "darkblue") +
  geom_text(aes(label = round(Average, 2)), vjust = -1, color = "blue") +
  scale_y_continuous(limits = c(0,4)) +
  labs(title = "Lipa City Annual Average Day-Night Band Radiance Values", x = "Year", y = "Average DNB Radiance Values") +
  theme_minimal()

##RASTER PLOT FOR EACH YEAR ----------------------------------------------------
textsize <- 6
p1 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,3])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2014") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
p2 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,4])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2015") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
p3 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,5])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2016") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
p4 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,6])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2017") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
p5 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,7])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2018") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
p6 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,8])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2019") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
p7 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,9])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2020") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
p8 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,10])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2021") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
p9 <- ggplot() +
  geom_raster(data = ntl, aes(x = longitude, y = latitude, fill = annual[,11])) +
  scale_fill_viridis_c() + labs(fill = "avg_rad") + coord_fixed() + ggtitle("2022") +
  theme(axis.text.x = element_text(size = textsize), axis.text.y = element_text(size = textsize))
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol=3,nrow=3)

##KERNEL DENSITY ESTIMATION OF NTL ---------------------------------------------
##NOTES: running spatstat would require the shapefiles and datasets to be transformed
##so spatstat can read it. 

#Reproject the shapefile to UTM zone 51N for PH
lipa_sf2 <- st_transform(sf, crs = 32651)

#Convert ntl df to sf object with the appropriate CRS
ntl_sf2 <- st_as_sf(ntl, coords = c("longitude", "latitude"), crs = 4326)
ntl_sf2 <- st_transform(ntl_sf2, crs = st_crs(lipa_sf2))

#Convert shape file an owin window for spatstat compatibility
lipa_owin <- as.owin(st_geometry(lipa_sf2))
#For added precaution, we consider points only within the shape
ntl_within_lipa <- st_intersection(ntl_sf2, lipa_sf2)
#Convert the points to a ppp object for spatstat compatibility
coords <- st_coordinates(ntl_within_lipa)
values <- ntl_within_lipa$avg_rad
ntl_ppp <- ppp(x = coords[, 1], y = coords[, 2], window = lipa_owin, marks = values)

#Perform KDE. bw.diggle(ntl_ppp) selects the most appropriate smoothing bandwidth according to Diggle(1985)
#The bandwidth used is 325.2057
bw = bw.diggle(ntl_ppp)
print(bw)
density_result <- density.ppp(ntl_ppp, weights = values, sigma = 325.2057, at = "pixels", edge = TRUE)
density_df <- as.data.frame(density_result)
colnames(density_df) <- c("latitude", "longitude", "z")

#Plot the intensity estimated by KDE
ggplot() +
  geom_raster(data = density_df, aes(x = latitude, y = longitude, fill = z)) +
  scale_fill_viridis_c() +
  geom_sf(data = lipa_sf2, fill = NA, color = "black") +
  coord_sf() +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  labs(title = "Kernel Density Estimation of avg_rad in Lipa City (sigma=325.2057)",
       fill = "Kernel Density Value") +
  theme_minimal()

density_result2 <- density.ppp(ntl_ppp, weights = values, sigma = 2*325.2057, at = "pixels", edge = TRUE)
density_df2 <- as.data.frame(density_result2)
colnames(density_df2) <- c("latitude", "longitude", "z")

#Plot the intensity estimated by KDE using twice the Diggle bandwidth
ggplot() +
  geom_raster(data = density_df2, aes(x = latitude, y = longitude, fill = z)) +
  scale_fill_viridis_c() +
  geom_sf(data = lipa_sf2, fill = NA, color = "black") +
  coord_sf() +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  labs(title = "Kernel Density Estimation of avg_rad in Lipa City (sigma = 650.4114)",
       fill = "Kernel Density Value") +
  theme_minimal()

#####SECTION 2 - CORRELATION BETWEEN NTL AND NDVI -----------------------------------------------------------------------------------------------------------------------------------------------------

##QUADRATS AND PEAERSON CORRELATION BETWEEN NTL AND NDVI -----------------------
n_rows <- 10
n_cols <- 10
simple_lipa_sf <- st_simplify(sf)

#Create a bounding box to define spatial extent of quadrats
bbox <- st_bbox(simple_lipa_sf)

#Creates the grid of polygons, create the polygons, and convert to an sf object
x_breaks <- seq(bbox["xmin"], bbox["xmax"], length.out = n_cols + 1)
y_breaks <- seq(bbox["ymin"], bbox["ymax"], length.out = n_rows + 1)
grid <- st_make_grid(simple_lipa_sf, n = c(n_cols, n_rows))
grid_sf <- st_sf(geometry = grid)
grid_sf$quadrat_id <- 1:nrow(grid_sf)
#Intersect the grid with the shape file and ensure both points and polygon share same coordinate system
quadrats <- st_intersection(grid_sf, simple_lipa_sf)

#The size of one quadrat fully within the polygon is 4.24 square km
quadrats$area <- st_area(quadrats)
quadrats$area <- quadrats$area/1e6
max(quadrats$area)

#Join points with the quadrats
ntlq <- st_transform(ntl_sf, crs(quadrats))
ndviq <- st_transform(vi_sf, crs(quadrats))
joined_sf_ntl <- st_join(ntlq, quadrats, join=st_intersects)
joined_sf_ndvi <- st_join(ndviq, quadrats, join=st_intersects)

#PRELIMARY CHECK: No point should be put in more than one quadrat. A sufficient check is that
#the number of rows in the original point dataset equal the number of rows in the data
#where points have their assigned quadrats. A point assigned to multiple quadrats will
#of course appear in multiple rows in the joined dataset

if (nrow(ntlq) == nrow(joined_sf_ntl)) {
  print("No NTL point is counted in multiple quadrats")
} else {
  print("There are NTL points counted in more than one quadrats")
}
if (nrow(ndviq) == nrow(joined_sf_ndvi)) {
  print("No NDVI point is counted in multiple quadrats")
} else {
  print("There are NDVI points counted in more than one quadrats")
}

#Group by quadrat ID and calculate average of pixels
averages_ntl <- joined_sf_ntl %>%
  group_by(quadrat_id) %>%
  summarise(avg_avg_rad = mean(avg_rad))
quadrats_with_avg_ntl <- quadrats %>% st_join(averages_ntl)
averages_ndvi <- joined_sf_ndvi %>%
  group_by(quadrat_id) %>%
  summarise(avg_ndvi = mean(NDVI_calculated))
quadrats_with_avg_ndvi <- quadrats %>% st_join(averages_ndvi)

#PRELIMINARY CHECK: Ensure that both datasets have the same number of quadrats
if (nrow(quadrats_with_avg_ntl) == nrow(quadrats_with_avg_ndvi)){
  cat("Number of quadrats for both datasets is", nrow(quadrats_with_avg_ntl))
} else {
  print("Unequal")
}


View(quad)
#Plot the Quadrat Values
quadrats_to_label <- c(45, 86, 96)
q1_data <- filter(quadrats_with_avg_ntl, quadrat_id.x %in% quadrats_to_label)
q2_data <- filter(quadrats_with_avg_ndvi, quadrat_id.x %in% quadrats_to_label)
q1 <- ggplot() +
  geom_sf(data = quadrats_with_avg_ntl, aes(fill = avg_avg_rad)) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "Mean NTL") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  theme_minimal() + 
  geom_sf_text(data = q1_data, aes(label = quadrat_id.x), size = 4, color = "black") +
  labs(title = "Mean NTL values per quadrat",
       x = "Longitude",
       y = "Latitude")
q2 <- ggplot() +
  geom_sf(data = quadrats_with_avg_ndvi, aes(fill = avg_ndvi)) +
  scale_fill_viridis_c(option = "viridis") +
  labs(fill = "Mean NDVI") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  theme_minimal() +
  geom_sf_text(data = q2_data, aes(label = quadrat_id.x), size = 4, color = "white") +
  labs(title = "Mean NDVI values per quadrat",
       x = "Longitude",
       y = "Latitude")
grid.arrange(q1,q2,ncol=2)

#Convert the two sf objects to dataframe since merge() does not support sf objects
df1 <- as.data.frame(quadrats_with_avg_ntl)
df2 <- as.data.frame(quadrats_with_avg_ndvi)

#Combine the two dataframes
combined <- merge(df1, df2, by = "quadrat_id.x")
#Remove unnecessary columns
simplified <- combined[c("quadrat_id.x","avg_avg_rad","avg_ndvi")]
#Viewing the simplified dataset reveals quadrats where there are no mean NTL value,
#mean NDVI value, or both. This is visually confirmed by the plots earlier.
#So we clean the dataset
cleaned <- simplified[complete.cases(simplified$avg_avg_rad, simplified$avg_ndvi), ]

#We are left with 61 quadrats
nrow(cleaned)
cor.test(cleaned$avg_avg_rad, cleaned$avg_ndvi)
View(cleaned)
#Scatterplot of Quadrat Averages of NTL and NDVI
plot(cleaned$avg_avg_rad, cleaned$avg_ndvi, 
     xlab = "Average NTL", ylab = "Average NDVI",
     main = "Scatterplot of Quadrat Averages of NTL and NDVI points",
     pch = 19)
abline(lm(avg_ndvi ~ avg_avg_rad, data = cleaned), col = "red", lwd=2)
summary(lm(avg_ndvi ~ avg_avg_rad, data = cleaned))
highlight_points <- cleaned$quadrat_id.x  %in% c(45, 86, 96)
points(cleaned$avg_avg_rad[highlight_points], cleaned$avg_ndvi[highlight_points], 
       col = "red", pch = 19)
text(cleaned$avg_avg_rad[highlight_points], cleaned$avg_ndvi[highlight_points], 
     labels = cleaned$quadrat_id.x[highlight_points], pos = 3, offset = 0.5, col = "black")

#ASIDE: Here is a function that iteratively runs the pearson r with different grid sizes.
#We see a decrease in value in R the smaller the quadrats, but all values remain negative.
#Note how ever that more quadrats can lead to some quadrats not fitting "well" in the polygon,
#causing a known error of "degenerate" shapes, particularly starting at 11x11 grids.

##CORRELATIONS AND DIFFERENT GRID SIZES ----------------------------------------
iter_corr_test <- function(n, results){
  n_rows <- n
  n_cols <- n
  simple_lipa_sf <- st_simplify(sf)
  bbox <- st_bbox(simple_lipa_sf)
  x_breaks <- seq(bbox["xmin"], bbox["xmax"], length.out = n_cols + 1)
  y_breaks <- seq(bbox["ymin"], bbox["ymax"], length.out = n_rows + 1)
  grid <- st_make_grid(simple_lipa_sf, n = c(n_cols, n_rows))
  grid_sf <- st_sf(geometry = grid)
  grid_sf$quadrat_id <- seq_len(nrow(grid_sf))
  quadrats <- st_intersection(grid_sf, simple_lipa_sf)
  ntlq <- st_transform(ntl_sf, crs(quadrats))
  ndviq <- st_transform(vi_sf, crs(quadrats))
  joined_sf_ntl <- st_join(ntlq, quadrats, join=st_intersects)
  joined_sf_ndvi <- st_join(ndviq, quadrats, join=st_intersects)
  averages_ntl <- joined_sf_ntl %>%
    group_by(quadrat_id) %>%
    summarise(avg_avg_rad = mean(avg_rad))
  quadrats_with_avg_ntl <- quadrats %>% st_join(averages_ntl)
  averages_ndvi <- joined_sf_ndvi %>%
    group_by(quadrat_id) %>%
    summarise(avg_ndvi = mean(NDVI_calculated))
  quadrats_with_avg_ndvi <- quadrats %>% st_join(averages_ndvi)
  df1 <- as.data.frame(quadrats_with_avg_ntl)
  df2 <- as.data.frame(quadrats_with_avg_ndvi)
  combined <- merge(df1, df2, by = "quadrat_id.x")
  simplified <- combined[c("quadrat_id.x","avg_avg_rad","avg_ndvi")]
  cleaned <- simplified[complete.cases(simplified$avg_avg_rad, simplified$avg_ndvi), ]
  quad <- nrow(cleaned)
  corre <- cor(cleaned$avg_avg_rad, cleaned$avg_ndvi)
  size <- as.character(n)
  quadrats$area <- st_area(quadrats)
  quadrats$area <- quadrats$area/1e6
  new_result <- data.frame(
    grid_size = paste(size,"x",size),
    quadrat_size = max(quadrats$area),
    num_filled_quadrats = quad,
    correlation = corre
  )
  return(rbind(results, new_result))
}

results <- data.frame()
for (n in 3:10) {
  results <- iter_corr_test(n, results)
}
for (n in 12:14) {
  results <- iter_corr_test(n, results)
}
print(results)


#####SECTION 3 - KRIGING OF NTL AND NDVI --------------------------------------------------------------------------------------------------------------------------------------------------------------

##NTL VARIOGRAM MODELING -------------------------------------------------------
#NTL is heavily skewed to the right, with skewness = 2.1972
Skew(ntl_sf$avg_rad)

ggplot(ntl_sf, aes(x = avg_rad)) + 
  geom_histogram(binwidth = 2, fill = "darkgray", color = "black") + 
  ggtitle("Histogram of avg_rad") +
  xlab("avg_rad") + 
  ylab("Frequency")

View(ntl_sf)
#We get the log of avg_rad (ln)
Skew(log(ntl_sf$avg_rad))
ntl_sf$log_ntl <- log(ntl_sf$avg_rad)
ggplot(ntl_sf, aes(x = log_ntl)) + 
  geom_histogram(binwidth = 1, fill = "darkgray", color = "black") + 
  ggtitle("Histogram of avg_rad") +
  xlab("log_ntl") + 
  ylab("Frequency")

#Candidate  Model
v0_ntl <- variogram(log_ntl~1,ntl_sf)
plot(v0_ntl)
v0_ntl

#Initial Guesses for other families of variograms
v0_ntl.fit1<-fit.variogram(v0_ntl,vgm(1,"Sph",6,1))
v0_ntl.fit1
plot(v0_ntl.fit1, 6)

v0_ntl.fit2<-fit.variogram(v0_ntl,vgm(1,"Exp",6,1))
v0_ntl.fit2
plot(v0_ntl.fit2, 6)

v0_ntl.fit3<-fit.variogram(v0_ntl,vgm(1,"Gau",6,1))
v0_ntl.fit3
plot(v0_ntl.fit3, 6)

v0_ntl.fit4<-fit.variogram(v0_ntl,vgm(1,"Lin",6,1))
v0_ntl.fit4
plot(v0_ntl.fit1, 6)

v0_ntl.fit5<-fit.variogram(v0_ntl,vgm(1,"Cir",6,1))
v0_ntl.fit5
plot(v0_ntl.fit1, 6)

#Overlay of Guesses on top of Candidate
plot(v0_ntl ,vgm(0.9642,"Sph",10.5870,0.00))
plot(v0_ntl ,vgm(1.9412,"Exp",13.0398,0.00))
plot(v0_ntl ,vgm(0.7220,"Gau",3.5748,0.0625))
plot(v0_ntl ,vgm(0.888,"Lin",7.1914,0.0117))
plot(v0_ntl ,vgm(0.9197,"Cir",8.6975,0.00))

#Select Model with lowest SSE
sse1 <- attr(v0_ntl.fit1, "SSErr")
sse2 <- attr(v0_ntl.fit2, "SSErr")
sse3 <- attr(v0_ntl.fit3, "SSErr")
sse4 <- attr(v0_ntl.fit4, "SSErr")
sse5 <- attr(v0_ntl.fit5, "SSErr")
cat("Spherical Model SSE:", sse1, "\n")
cat("Exponential Model SSE:", sse2, "\n")
cat("Gaussian Model SSE:", sse3, "\n")
cat("Linear Model SSE:", sse4, "\n")
cat("Circular Model SSE:", sse5, "\n")
#Spherical Model for log_ntl has lowest SSE, 6.960461


crs_info <- st_crs(ntl_sf)

# Print the CRS information
print(crs_info)

is_projected <- crs_info$proj4string != "+proj=longlat +datum=WGS84 +no_defs"
if (is_projected) {
  cat("The data is projected.\n")
} else {
  cat("The data is in geographic coordinates (longitude/latitude).\n")
}


##NDVI VARIOGRAM MODELING ------------------------------------------------------
Skew(vi_sf$NDVI_calculated)
#Candidate  Model
v0_ndvi <- variogram(NDVI_calculated~1,vi_sf)
plot(v0_ndvi)
v0_ndvi

#Initial Guesses for other families of variograms
v0_ndvi.fit1<-fit.variogram(v0_ndvi,vgm(1,"Sph",6,1))
v0_ndvi.fit1
plot(v0_ndvi.fit1, 6)

v0_ndvi.fit2<-fit.variogram(v0_ndvi,vgm(1,"Exp",6,1))
v0_ndvi.fit2
plot(v0_ndvi.fit2, 6)

v0_ndvi.fit3<-fit.variogram(v0_ndvi,vgm(1,"Gau",6,1))
v0_ndvi.fit3
plot(v0_ndvi.fit3, 6)

v0_ndvi.fit4<-fit.variogram(v0_ndvi,vgm(1,"Lin",6,1))
v0_ndvi.fit4
plot(v0_ndvi.fit1, 6)

v0_ndvi.fit5<-fit.variogram(v0_ndvi,vgm(1,"Cir",6,1))
v0_ndvi.fit5
plot(v0_ndvi.fit1, 6)

#Overlay of Guesses on top of Candidate
plot(v0_ndvi ,vgm(0.0114,"Sph",3.6190,0.0105))
plot(v0_ndvi ,vgm(0.01412,"Exp",1.4223,0.0084))
plot(v0_ndvi ,vgm(0.0099,"Gau",1.3431,0.01126))
plot(v0_ndvi ,vgm(0.0107,"Lin",3.0362,0.0114))
plot(v0_ndvi ,vgm(0.0112,"Cir",3.2515,0.0108))

#Select Model with lowest SSE
sse11 <- attr(v0_ndvi.fit1, "SSErr")
sse22 <- attr(v0_ndvi.fit2, "SSErr")
sse33 <- attr(v0_ndvi.fit3, "SSErr")
sse44 <- attr(v0_ndvi.fit4, "SSErr")
sse55 <- attr(v0_ndvi.fit5, "SSErr")
cat("Spherical Model SSE:", sse11, "\n")
cat("Exponential Model SSE:", sse22, "\n")
cat("Gaussian Model SSE:", sse33, "\n")
cat("Linear Model SSE:", sse44, "\n")
cat("Circular Model SSE:", sse55, "\n")
#Exponential Model for NDVI_calculated has lowest SSE: 0.01008248 

#THE KRIGED PREDICTIONS AND STANDARD ERRORS ARE GIVEN BY

##KRIGING ----------------------------------------------------------------------
#Define the final variogram models:
v_ntl <- vgm(psill = 0.9642, model = "Sph", range = 10.5870, nugget = 0.00)
v_ndvi <- vgm(psill = 0.01412, model = "Exp", range = 1.4223, nugget = 0.0084)
#Create the prediction grid for kriging
xmin <- min(ntl$longitude)
xmax <- max(ntl$longitude)
ymin <- min(ntl$latitude)
ymax <- max(ntl$latitude)
resolution <- 0.001  # Specify the resolution of the grid (adjust as needed)

# Create a regular grid of points, then intersect with Lipa Shape File
prediction_grid <- expand.grid(
  longitude = seq(xmin, xmax, by = resolution),
  latitude = seq(ymin, ymax, by = resolution)
)
prediction_grid_sf <- st_as_sf(prediction_grid, coords = c("longitude", "latitude"), crs = st_crs(ntl_sf))
prediction_grid_within_lipa <- st_intersection(prediction_grid_sf, lipa_sf)
nrow(prediction_grid_within_lipa)

#Kriging over the points
ntl_sf$log_ntl <- log(ntl_sf$avg_rad)
kriged_ntl<-krige(log_ntl~1,locations=ntl_sf,prediction_grid_within_lipa,v_ntl) 
kriged_ndvi<-krige(NDVI_calculated~1,locations=vi_sf,prediction_grid_within_lipa,v_ndvi) 

coords <- st_coordinates(prediction_grid_within_lipa)
coords_df <- as.data.frame(coords)
names(coords_df) <- c("longitude", "latitude")

kriged_ntl_lipa <- cbind(coords_df, kriged_ntl$var1.pred)
kriged_ndvi_lipa <- cbind(coords_df, kriged_ndvi$var1.pred)
names(kriged_ntl_lipa) <- c("longitude","latitude","pred_ntl_log")
names(kriged_ndvi_lipa) <- c("longitude","latitude","pred_ndvi")
kriged_ntl_lipa$pred_ntl_antilog <- exp(kriged_ntl_lipa$pred_ntl)
final_krig <- cbind(kriged_ntl_lipa, kriged_ndvi_lipa[,"pred_ndvi"])
names(final_krig) <- c("longitude","latitude","pred_ntl","pred_ntl_antilog","pred_ndvi")
View(final_krig)

#Final dataset with antilogged pred_ntl and pred_ndvi
kriged_ntl_lipa_sf <- st_as_sf(kriged_ntl_lipa, coords = c("longitude", "latitude"), crs = 4326)
kriged_ntl_lipa_sf$pred_ntl_antilog <- exp(kriged_ntl_lipa_sf$pred_ntl)
kriged_ndvi_lipa_sf <- st_as_sf(kriged_ndvi_lipa, coords = c("longitude", "latitude"), crs = 4326)

# Plot using ggplot2
g1 <- ggplot(kriged_ntl_lipa, aes(x = longitude, y = latitude, color = pred_ntl_antilog)) +
  geom_point() +
  scale_color_viridis_c(name = "predicted avg_rad") +  
  labs(title = "Predicted NTL over Lipa City (15982 points)") +
  annotation_scale(location = "bl", width_hint = 0.2, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  theme_minimal()
g2 <- ggplot(kriged_ndvi_lipa, aes(x = longitude, y = latitude, color = pred_ndvi)) +
  geom_point() +
  scale_color_viridis_c(name = "predicted NDVI_calculated") +  
  labs(title = "Predicted NDVI over Lipa City (15982 points)") +
  annotation_scale(location = "bl", width_hint = 0.2, bar_cols = c("black", "white")) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  theme_minimal()
grid.arrange(g1,g2,ncol=2)

##CROSS-VALIDATION WITH OBSERVED DATA ------------------------------------------
#For NTL: sum of squared residuals is 20.81242
cv_ntl <- krige.cv(log(avg_rad)~1, data=ntl, locations=~longitude+latitude, model=v_ntl, nfold=nrow(ntl))
sum(cv_ntl$residual^2) 
View(cv_ntl)
#In computing for residuals, the observed avg_rad values are also log-transformed since the predictions
#are log-transformed. Residuals are just the difference then. 

#For NDVI: sum of squared residuals is 15.29765
cv_ndvi <- krige.cv(NDVI_calculated~1, data=vi, locations=~longitude+latitude, model=v_ndvi, nfold=nrow(vi))
sum(cv_ndvi$residual^2) 
View(cv_ndvi)


#####SECTION 4 - MODELING INTENSITY OF LEARNING SPACES ------------------------------------------------------------------------------------------------------------------------------------------------

##COMPUTING DISTANCE TO NEAREST SCHOOL -----------------------------------------
#Import Datasets
poi <- read.csv("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/poi/learning_spaces.csv")
schools <- read.csv("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/poi/schools.csv")
View(poi)
View(schools)
nrow(poi) #354 points of interest
nrow(schools) #43 schools

#Convert to sf objects
poi_sf <- st_as_sf(poi, coords = c("Longitude", "Latitude"), crs = 4326)
schools_sf <- st_as_sf(schools, coords = c("Longitude", "Latitude"), crs = 4326)

# Ensure both sf objects have the same CRS
poi_sf <- st_transform(poi_sf, crs = st_crs(lipa_sf))
schools_sf <- st_transform(schools_sf, crs = st_crs(lipa_sf))

# Compute distance matrix between each POI and all schools. This creates 354x43 matrix
distances <- st_distance(poi_sf, schools_sf)
View(distances)

# Find the minimum distance (in meters) for each POI
min_distances <- apply(distances, 1, min)

# Add the minimum distance to the poi_sf object
poi_sf$dist_nearest_school <- min_distances
sum(poi_sf$dist_nearest_school == 0)
#Location of school with overlapping caffe (121.1559, 13.94114)
head(poi_sf)

##PREDICTION OF ADDED POINTS ---------------------------------------------------
#Run Kriging on the points for NTL
predicted_log_ntl <- krige(log_ntl ~ 1, locations = ntl_sf, newdata = poi_sf, model = v_ntl)
#Combine points location with predicted
predictions <- cbind(poi_sf,predicted_log_ntl$var1.pred)
#Compute antilog of predicted ntl
predictions$pred_ntl_antilog <- exp(predicted_log_ntl$var1.pred)
View(predictions)
#Run Kriging on the points for NDVI
predicted_ndvi <- krige(NDVI_calculated ~ 1,locations = vi_sf, newdata = poi_sf, model = v_ndvi)
#Append the results of predicted ndvi
predictions <- cbind(predictions,predicted_ndvi[1]) 

#Create final dataset and export 
final <- predictions[, c("Code", "dist_nearest_school", "pred_ntl_antilog", "var1.pred", "geometry")]
colnames(final)[colnames(final) == "var1.pred"] <- "pred_ndvi"
final <- st_as_sf(final, coords = "geometry")
final$longitude <- st_coordinates(final$geometry)[, "X"]
final$latitude <- st_coordinates(final$geometry)[, "Y"]
final$geometry <- NULL
write.csv(final, file = "D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/poi/final.csv", row.names = FALSE)

summary(final)
sd(final$dist_nearest_school)
sd(final$pred_ntl_antilog)
sd(final$pred_ndvi)
Skew(final$dist_nearest_school)
Skew(final$pred_ntl_antilog)
Skew(final$pred_ndvi)


##MODELING PROPER --------------------------------------------------------------

##TESTING FOR CLUSTERING OF LEARNING SPACES POI --------------------------------
final <- read.csv("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/poi/final.csv")
final_sf <- st_as_sf(final, coords = c("longitude", "latitude"), crs = st_crs(lipa_sf))

#Transform lipa shape file to a project coordinates
lipa_proj <- st_transform(lipa_sf, crs = 32651)  # Replace 32651 with the appropriate EPSG code for your area

#Transform final to the same projected CRS
final_sf <- st_as_sf(final, coords = c("longitude", "latitude"), crs = st_crs(4326))
final_proj <- st_transform(final_sf, crs = st_crs(lipa_proj))

#Convert lipa_proj to an owin object
lipa_owin <- as.owin(lipa_proj)

#Extract coordinates from final_proj
coords <- st_coordinates(final_proj)

#Convert to ppp object with the polygon as the observation window
final_ppp <- ppp(x = coords[, 1], y = coords[, 2], window = lipa_owin)

#For a guard correction, create a smaller version of lipa_proj as the by buffering inward
#So essentially, the guard is a smaller version of lipa city
buffer_distance <- -500 
clipregion <- st_buffer(lipa_proj, dist = buffer_distance)
clipregion_owin <- as.owin(clipregion)
ggplot() +
  geom_sf(data = lipa_proj, aes(fill = "Original Polygon")) +  
  geom_sf(data = clipregion, aes(fill = "Guard Area", color="red"), alpha = 0.5) +         
  scale_fill_manual(name = "Regions", values = c("Original Polygon" = "white", "Guard Area" = "#ffded6")) +
  scale_color_manual(name = "Regions", values = c("Original Polygon" = "black", "Guard Area" = "red")) +
  theme_minimal() +
  ggtitle("Lipa City and Guard Area for Edge Correction") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(hjust = 0.5))

#Run Clark-Evans Test with H1: clustered 
cet <- clarkevans.test(final_ppp, alternative = "clustered", correction = "guard", clipregion = clipregion_owin)
print(cet)

#Run K-function test with simulation envelopes
Env <- envelope(final_ppp,Kest,nsim=999,rank=1) 
write.csv(Env, file = "D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/poi/k-function test.csv", row.names = FALSE)
plot(Env, main = "Global K-function Envelope for Learning Spaces POI with 999 Simulations")

##COVARIATE SPATIAL IMAGE PREPARATION ------------------------------------------
sf <- st_read("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/Nighttime Lights/CityOfLipa_shape.shp")
lipa_sf <- st_geometry(sf)
lipa_sf <- st_transform(lipa_sf, crs = 4326)
final <- read.csv("D:/UPD Files/4th Year/2nd Sem/Stat 197 WFX - Spatial Data Analysis/Paper/poi/final.csv")
data <- final
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

# Project to a suitable projected coordinate system, e.g., UTM Zone 51N
lipa_sf_proj <- st_transform(lipa_sf, crs = 32651)
# Transform learning spaces to the same projected CRS
data_sf_proj <- st_transform(data_sf, crs = 32651)
# Convert to spatstat ppp object
window <- as.owin(lipa_sf_proj)
data_ppp <- as.ppp(st_coordinates(data_sf_proj), window)
plot(data_ppp)

# transforming NTL to image
ntl <- cbind(data$longitude,data$latitude,data$pred_ntl_antilog)
colnames(ntl) <- c("x","y","ntl")
ntl <- as.data.frame(ntl)
ntl_sf <- st_as_sf(ntl, coords = c("x", "y"), crs = 4326)
ntl_sf_proj <- st_transform(ntl_sf, crs = 32651)
ntl_ppp <- as.ppp(ntl_sf_proj,W=window)
NTL <- Smooth(ntl_ppp)

# transforming NDVI to image
ndvi <- cbind(data$longitude,data$latitude,data$pred_ndvi)
colnames(ndvi) <- c("x","y","ndvi")
ndvi <- as.data.frame(ndvi)
ndvi_sf <- st_as_sf(ndvi, coords = c("x", "y"), crs = 4326)
ndvi_sf_proj <- st_transform(ndvi_sf, crs = 32651)
ndvi_ppp <- as.ppp(ndvi_sf_proj,W=window)
NDVI <- Smooth(ndvi_ppp)

# transforming DIST to image
dist <- cbind(data$longitude,data$latitude,data$dist_nearest_school)
colnames(dist) <- c("x","y","dist")
dist <- as.data.frame(dist)
dist_sf <- st_as_sf(dist, coords = c("x", "y"), crs = 4326)
dist_sf_proj <- st_transform(dist_sf, crs = 32651)
dist_ppp <- as.ppp(dist_sf_proj,W=window)
DistNS <- Smooth(dist_ppp)







