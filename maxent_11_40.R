# Load necessary libraries
library(dismo)
library(raster)
library(rJava)
library(ggplot2)
library(sf)
library(mapview)
library(sp)
library(car)
library(htmlwidgets)
set.seed(23)
# Data with Bioclimate Variables
file_urls_11_40 <- c("/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio1_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio2_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio3_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio4_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio5_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio6_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio7_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio8_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio9_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio10_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio11_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio12_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio13_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio14_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio15_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio16_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio17_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio18_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2011_2040/CHELSA_bio19_2011-2040_gfdl-esm4_ssp126_V.2.1.tif")
# Download and load the files of Bioclimate Variables
raster_objects_11_40 <- lapply(file_urls_11_40, raster)
# Combine the list of raster layers into a stack of Bioclimate Variables
raster_stack_11_40 <- stack(raster_objects_11_40)
# Download the Natural Earth shapefile for the United States
url <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_500k.zip"
download.file(url, destfile = "cb_2020_us_state_500k.zip")
unzip("cb_2020_us_state_500k.zip", exdir = "shapefiles")
# Read the shapefile
usa <- st_read("shapefiles/cb_2020_us_state_500k.shp")
# Subset for Wyoming
wyoming_boundary <- usa[usa$STUSPS == "WY", ]
# Create a boundary for Wyoming
WYO_boundary <- st_bbox(wyoming_boundary)
# Clip the raster stack to the extent of the Wyoming boundary of Bioclimate Variables
raster_stack_clipped_11_40 <- crop(raster_stack_11_40, WYO_boundary)
# Save the cropped raster stack as an RDS file
saveRDS(raster_stack_clipped_11_40, file = "raster_stack_clipped_11_40.rds")
# Load the cropped raster stack as an RDS file
raster_stack_clipped_11_40 <- readRDS("raster_stack_clipped_11_40.rds")
# Slope, Surface Geology, TWI and aspect files
other_pred <- c("/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/slope.tif",
                "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/SurficialGeology.tif",
                "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/TWI.tif",
                "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/aspect.tif")
# Download and load the other files
other_raster_objects <- lapply(other_pred, raster)
# Combine the list of other raster layers into a stack
other_raster_stack <- stack(other_raster_objects)
# Loop through each raster layer in the list to have the same rasters
for (i in seq_along(other_raster_objects)) {
  # Get the raster layer
  raster_layer <- other_raster_objects[[i]]
  # Project raster to match the extent and resolution of raster_stack_clipped
  raster_layer <- projectRaster(raster_layer, raster_stack_clipped_11_40)
  # Assign the modified raster layer back to the list
  other_raster_objects[[i]] <- raster_layer
}
# Stack the adjusted raster layers
other_raster_stack <- stack(other_raster_objects)
saveRDS(other_raster_stack, "other_raster_stack.rds")
other_raster_stack <- readRDS("other_raster_stack.rds")
# Final stacked raster for 1981-2010
final_raster_stack_11_40 <- stack(raster_stack_clipped_11_40, other_raster_stack)
saveRDS(final_raster_stack_11_40, "final_raster_stack_11_40.rds")
final_raster_stack_11_40 <- readRDS("final_raster_stack_11_40.rds")
# Sample occurrence data (replace this with your actual occurrence data)
#maxent_data <- read.csv("/Users/oisinogailin/Desktop/spatial/finalProject/spatial/maxent_data.csv") # sensitive data
# Convert the obsdate column to Date type if it's not already
maxent_data$obsdate <- as.Date(maxent_data$obsdate, format="%Y/%m/%d")
# Keep rows with obsdate after 2019/01/01
maxent_data <- subset(maxent_data, obsdate > as.Date("2010/01/01"))
# Withholding a 30% sample for testing
fold <- kfold(maxent_data, k=3)
occtest <- maxent_data[fold == 1, c("longitude", "latitude")]
occtrain <- maxent_data[fold != 1, c("longitude", "latitude")]
# Add in psuedo absences
# Create a SpatialPoints object within the bounding box
pseudo_absences <- spsample(as(extent(WYO_boundary), "SpatialPolygons"), n = nrow(occtrain), type = "random")
pseudo_absence_values_11_40 <- extract(final_raster_stack_11_40, pseudo_absences)
pseudo_absence_data_11_40 <- data.frame(pseudo_absences, pseudo_absence_values_11_40)[,1:2]
colnames(pseudo_absence_data_11_40) <- c("longitude", "latitude")
maxent_data_combined_11_40 <- rbind(occtrain, pseudo_absence_data_11_40)
# Fit MaxEnt model with pseudo absences
me_11_40 <- maxent(x = final_raster_stack_11_40, p = maxent_data_combined_11_40)
# Fit MaxEnt model with no pseudo absences
# me <- maxent(x = final_raster_stack_11_40, p = occtrain)
# Display MaxEnt results in a browser
me_11_40
# Plot showing importance of each variable
# Save the MaxEnt plot
pdf("maxent_variable_importance_plot_11_40.pdf")  # Open a PDF file to save the plot
plot(me_11_40)
dev.off()  # Close the PDF file
# Predict to the entire dataset
r_11_40 <- predict(me_11_40, final_raster_stack_11_40)
# Plot the prediction
pdf("maxent_prediction_plot_11_40.pdf")  # Open a PDF file to save the plot
plot(r_11_40)
points(occtrain)
dev.off()
occtrain <- na.omit(occtrain)
occtrain_sf <- st_as_sf(occtrain, coords = c("longitude", "latitude"), crs = st_crs(r_11_40))
# Assuming 'occtrain' is a data frame containing training points
# Assuming 'r' is the MaxEnt prediction raster layer
map_11_40 <- mapview(r_11_40) + mapview(occtrain_sf, color = "white", size = 0.01, legend = TRUE, col.regions = "snow", alpha.regions = 0.1)
# Save the map as a PNG file using mapshot
map_save_path_11_40 <- "maxent_prediction_map_11_40.png"
webshot::install_phantomjs()
mapshot(map_11_40, file = map_save_path_11_40)
# 3.3 Model evaluation To evaluate models, we use the evaluate function from the "dismo" package. 
coordinates(occtrain) <- ~longitude + latitude # replace 'longitude' and 'latitude' with actual column names
coordinates(pseudo_absence_data_11_40) <- ~longitude + latitude # replace 'longitude' and 'latitude' with actual column names
# Evaluation indices include AUC, TSS, Sensitivity, Specificity, etc.
p_11_40 <- extract(final_raster_stack_11_40, occtrain)
a_11_40 <- extract(final_raster_stack_11_40, pseudo_absence_data_11_40)
### model eval
mod_eval_train_11_40 <- dismo::evaluate(p = p_11_40, a = a_11_40, model = me_11_40)
# calculate thresholds of models
thd1_11_40 <- threshold(mod_eval_train_11_40, "no_omission")  # 0% omission rate 
thd2_11_40 <- threshold(mod_eval_train_11_40, "spec_sens")  # highest TSS
# Save thd2_11_40 value to a text file
thd2_save_path_11_40 <- "maxent_thd2_11_40.txt"
write(thd2_11_40, file = thd2_save_path_11_40)
# plotting points that are above the previously calculated
# thresholded value
threshold_plot_11_40 <- plot(r_11_40 >= thd2_11_40)
# Save the threshold plot
pdf("maxent_threshold_plot_11_40.pdf")
threshold_plot_11_40
dev.off()
