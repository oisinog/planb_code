
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
set.seed(3)
# Data with Bioclimate Variables
file_urls_41_70 <- c("/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio1_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio2_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio3_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio4_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio5_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio6_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio7_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio8_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio9_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio10_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio11_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio12_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio13_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio14_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio15_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio16_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio17_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio18_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
                     "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/bioclim_2041-2070/CHELSA_bio19_2041-2070_gfdl-esm4_ssp126_V.2.1.tif")
# Download and load the files of Bioclimate Variables
raster_objects_41_70 <- lapply(file_urls_41_70, raster)
# Combine the list of raster layers into a stack of Bioclimate Variables
raster_stack_41_70 <- stack(raster_objects_41_70)
# Clip the raster stack to the extent of the Wyoming boundary of Bioclimate Variables
raster_stack_clipped_41_70 <- crop(raster_stack_41_70, WYO_boundary)
# Save the cropped raster stack as an RDS file
saveRDS(raster_stack_clipped_41_70, file = "raster_stack_clipped_41_70.rds")
# Load the cropped raster stack as an RDS file
raster_stack_clipped_41_70 <- readRDS("raster_stack_clipped_41_70.rds")
# Slope, Surface Geology, TWI and aspect files
other_pred <- c("/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/slope.tif",
                "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/SurficialGeology.tif",
                "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/TWI.tif",
                "/Users/oisinogailin/Dropbox/Proj_UWStats_RareAnimalSDMs_2023/Data/aspect.tif")
other_raster_stack <- readRDS("other_raster_stack.rds")
# Final stacked raster for 1981-2010
final_raster_stack_41_70 <- stack(raster_stack_clipped_41_70, other_raster_stack)
saveRDS(final_raster_stack_41_70, "final_raster_stack_41_70.rds")
final_raster_stack_41_70 <- readRDS("final_raster_stack_41_70.rds")
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
pseudo_absence_values_41_70 <- extract(final_raster_stack_41_70, pseudo_absences)
pseudo_absence_data_41_70 <- data.frame(pseudo_absences, pseudo_absence_values_41_70)[,1:2]
colnames(pseudo_absence_data_41_70) <- c("longitude", "latitude")
maxent_data_combined_41_70 <- rbind(occtrain, pseudo_absence_data_41_70)
# Fit MaxEnt model with pseudo absences
me_41_70 <- maxent(x = final_raster_stack_41_70, p = maxent_data_combined_41_70)
# Fit MaxEnt model with no pseudo absences
# me <- maxent(x = final_raster_stack__41_70, p = occtrain)
# Display MaxEnt results in a browser
me_41_70
# Plot showing importance of each variable
# Save the MaxEnt plot
pdf("maxent_variable_importance_plot_41_70.pdf")  # Open a PDF file to save the plot
plot(me_41_70)
dev.off()  # Close the PDF file
# Predict to the entire dataset
r_41_70 <- predict(me_41_70, final_raster_stack_41_70)
# Plot the prediction
pdf("maxent_prediction_plot_41_70.pdf")  # Open a PDF file to save the plot
plot(r_41_70)
points(occtrain)
dev.off()
occtrain <- na.omit(occtrain)
occtrain_sf <- st_as_sf(occtrain, coords = c("longitude", "latitude"), crs = st_crs(r_41_70))
# Assuming 'occtrain' is a data frame containing training points
# Assuming 'r' is the MaxEnt prediction raster layer
map_41_70 <- mapview(r_41_70) + mapview(occtrain_sf, color = "white", size = 0.01, legend = TRUE, col.regions = "snow", alpha.regions = 0.1)
# Save the map as a PNG file using mapshot
map_save_path_41_70 <- "maxent_prediction_map_41_70.png"
webshot::install_phantomjs()
mapshot(map_41_70, file = map_save_path_41_70)
# 3.3 Model evaluation To evaluate models, we use the evaluate function from the "dismo" package. 
coordinates(occtrain) <- ~longitude + latitude # replace 'longitude' and 'latitude' with actual column names
coordinates(pseudo_absence_data_41_70) <- ~longitude + latitude # replace 'longitude' and 'latitude' with actual column names
# Evaluation indices include AUC, TSS, Sensitivity, Specificity, etc.
p_41_70 <- extract(final_raster_stack_41_70, occtrain)
a_41_70 <- extract(final_raster_stack_41_70, pseudo_absence_data_41_70)
### model eval
mod_eval_train_41_70 <- dismo::evaluate(p = p_41_70, a = a_41_70, model = me_41_70)
# calculate thresholds of models
thd1_41_70 <- threshold(mod_eval_train_41_70, "no_omission")  # 0% omission rate 
thd2_41_70 <- threshold(mod_eval_train_41_70, "spec_sens")  # highest TSS
# Save thd2_41_70 value to a text file
thd2_save_path_41_70 <- "maxent_thd2_41_70.txt"
write(thd2_41_70, file = thd2_save_path_41_70)
# plotting points that are above the previously calculated
# thresholded value
threshold_plot_41_70 <- plot(r_41_70 >= thd2_41_70)
# Save the threshold plot
pdf("maxent_threshold_plot_41_70.pdf")
threshold_plot_41_70
dev.off()

