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
set.seed(123)
# Data with Bioclimate Variables
file_urls_81_10 <- c(
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio1_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio2_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio3_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio4_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio5_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio6_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio7_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio8_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio9_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio10_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio11_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio12_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio13_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio14_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio15_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio16_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio17_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio18_1981-2010_V.2.1.tif",
  "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_1981_2010/CHELSA_bio19_1981-2010_V.2.1.tif")
# Download and load the files of Bioclimate Variables
raster_objects_81_10 <- lapply(file_urls_81_10, raster)
# Combine the list of raster layers into a stack of Bioclimate Variables
raster_stack_81_10 <- stack(raster_objects_81_10)
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
raster_stack_clipped_81_10 <- crop(raster_stack_81_10, WYO_boundary)
# Save the cropped raster stack as an RDS file
saveRDS(raster_stack_clipped_81_10, file = "raster_stack_clipped_81_10.rds")
# Load the cropped raster stack as an RDS file
raster_stack_clipped_81_10 <- readRDS("raster_stack_clipped_81_10.rds")
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
  raster_layer <- projectRaster(raster_layer, raster_stack_clipped_81_10)
  # Assign the modified raster layer back to the list
  other_raster_objects[[i]] <- raster_layer
}
# Stack the adjusted raster layers
other_raster_stack <- stack(other_raster_objects)
saveRDS(other_raster_stack, "other_raster_stack.rds")
other_raster_stack <- readRDS("other_raster_stack.rds")
# Final stacked raster for 1981-2010
final_raster_stack_81_10 <- stack(raster_stack_clipped_81_10, other_raster_stack)
saveRDS(final_raster_stack_81_10, "final_raster_stack_81_10.rds")
final_raster_stack_81_10 <- readRDS("final_raster_stack_81_10.rds")
# Sample occurrence data (replace this with your actual occurrence data)
#maxent_data <- read.csv("/Users/oisinogailin/Desktop/spatial/finalProject/spatial/maxent_data.csv") # sensitive data
maxent_data <- read.csv("Thomomys_clusius_Wyoming_Pocket_Gopher_precise_observations_statewide.csv")
# Convert the obsdate column to Date type if it's not already
maxent_data$obsdate <- as.Date(maxent_data$obsdate, format="%Y/%m/%d")
# Keep rows with obsdate after 2010/01/01
maxent_data <- subset(maxent_data, obsdate > as.Date("2010/01/01"))
# Withholding a 30% sample for testing
fold <- kfold(maxent_data, k=3)
occtest <- maxent_data[fold == 1, c("longitude", "latitude")]
occtrain <- maxent_data[fold != 1, c("longitude", "latitude")]
# Add in psuedo absences
# Create a SpatialPoints object within the bounding box
pseudo_absences <- spsample(as(extent(WYO_boundary), "SpatialPolygons"), n = nrow(occtrain), type = "random")
pseudo_absence_values_81_10 <- extract(final_raster_stack_81_10, pseudo_absences)
pseudo_absence_data_81_10 <- data.frame(pseudo_absences, pseudo_absence_values_81_10)[,1:2]
colnames(pseudo_absence_data_81_10) <- c("longitude", "latitude")
maxent_data_combined_81_10 <- rbind(occtrain, pseudo_absence_data_81_10)
# Fit MaxEnt model with pseudo absences
me_81_10 <- maxent(x = final_raster_stack_81_10, p = maxent_data_combined_81_10)
# Fit MaxEnt model with no pseudo absences
# me <- maxent(x = final_raster_stack_81_10, p = occtrain)
# Display MaxEnt results in a browser
me_81_10
# Plot showing importance of each variable
# Save the MaxEnt plot
pdf("maxent_variable_importance_plot_81_10.pdf")  # Open a PDF file to save the plot
plot(me_81_10)
dev.off()  # Close the PDF file
# Predict to the entire dataset
r_81_10 <- predict(me_81_10, final_raster_stack_81_10)
plot(r_81_10)
# Plot the prediction
pdf("maxent_prediction_plot_81_10.pdf")  # Open a PDF file to save the plot
plot(r_81_10)
points(occtrain)
dev.off()
occtrain <- na.omit(occtrain)
occtrain_sf <- st_as_sf(occtrain, coords = c("longitude", "latitude"), crs = st_crs(r_81_10))
# Assuming 'occtrain' is a data frame containing training points
# Assuming 'r' is the MaxEnt prediction raster layer
map_81_10 <- mapview(r_81_10) + mapview(occtrain_sf, color = "white", size = 0.01, legend = TRUE, col.regions = "snow", alpha.regions = 0.1)
# Save the map as a PNG file using mapshot
map_save_path_81_10 <- "maxent_prediction_map_81_10.png"
webshot::install_phantomjs()
mapshot(map_81_10, file = map_save_path_81_10)
# 3.3 Model evaluation To evaluate models, we use the evaluate function from the "dismo" package. 
coordinates(occtrain) <- ~longitude + latitude # replace 'longitude' and 'latitude' with actual column names
coordinates(pseudo_absence_data_81_10) <- ~longitude + latitude # replace 'longitude' and 'latitude' with actual column names
# Evaluation indices include AUC, TSS, Sensitivity, Specificity, etc.
p_81_10 <- extract(final_raster_stack_81_10, occtrain)
a_81_10 <- extract(final_raster_stack_81_10, pseudo_absence_data_81_10)
### model eval
mod_eval_train_81_10 <- dismo::evaluate(p = p_81_10, a = a_81_10, model = me_81_10)
# calculate thresholds of models
thd1_81_10 <- threshold(mod_eval_train_81_10, "no_omission")  # 0% omission rate 
thd2_81_10 <- threshold(mod_eval_train_81_10, "spec_sens")  # highest TSS
# Save thd2_81_10 value to a text file
thd2_save_path_81_10 <- "maxent_thd2_81_10.txt"
write(thd2_81_10, file = thd2_save_path_81_10)
# plotting points that are above the previously calculated
# thresholded value
threshold_plot_81_10 <- plot(r_81_10 >= thd2_81_10)
# Save the threshold plot
pdf("maxent_threshold_plot_81_10.pdf")
threshold_plot_81_10
dev.off()

