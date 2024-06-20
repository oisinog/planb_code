
set.seed(23)
# Extract predictor variables from the raster stack for presence points
xy <- maxent_data[, c("longitude", "latitude")]
cell_indices <- raster::extract(final_raster_stack_11_40, xy)
colnames(cell_indices) <- c(paste0("bio", 1:19), "slope", "SurficialGeology", "TWI", "aspect")
cell_indices <- data.frame(cell_indices)

# Calculate the correlation matrix
pair_matrix <- cor(na.omit(cell_indices))

# Initialize an empty list to store all pairs
all_pairs <- list()

# Iterate through each element in the correlation matrix
for (i in 1:(ncol(pair_matrix) - 1)) {
  for (j in (i + 1):ncol(pair_matrix)) {
    # Capture all pairs
    all_pairs[[length(all_pairs) + 1]] <- c(rownames(pair_matrix)[i], colnames(pair_matrix)[j])
  }
}

# Convert the list of all pairs to a data frame for display
all_pairs <- do.call(rbind, all_pairs)
colnames(all_pairs) <- c("Variable1", "Variable2")

# Withholding a 30% sample for testing
set.seed(123) # for reproducibility
fold <- kfold(maxent_data, k = 3)
occtest <- maxent_data[fold == 1, c("longitude", "latitude")]
occtrain <- maxent_data[fold != 1, c("longitude", "latitude")]

# Extract predictor variables for training data
# 3.3 Model evaluation To evaluate models, we use the evaluate function from the "dismo" package. 
coordinates(occtrain) <- ~longitude + latitude # replace 'longitude' and 'latitude' with actual column names
train_data_values <- extract(final_raster_stack_11_40, occtrain)
train_data <- data.frame(pa = 1, train_data_values)

# Add pseudo-absences
occtrain <- data.frame(na.omit(occtrain))
pseudo_absences <- spsample(as(extent(WYO_boundary), "SpatialPolygons"), n = nrow(occtrain), type = "random")
pseudo_absence_values <- extract(final_raster_stack_11_40, pseudo_absences)
pseudo_absence_data <- data.frame(pa = 0, pseudo_absence_values)

# Combine occurrence and pseudo-absence data
train_data <- rbind(train_data, pseudo_absence_data)

# Ensure column names are set correctly
colnames(train_data) <- c("pa", paste0("bio", 1:19), "slope", "SurficialGeology", "TWI", "aspect")

# Fit logistic regression models for each valid pair
models <- list()
predictions <- matrix(0, nrow = nrow(train_data), ncol = length(all_pairs))

for (i in seq_along(all_pairs)) {
  pair <- all_pairs[[i]]
  formula <- as.formula(paste("pa ~", paste(pair, collapse = " + ")))
  model <- glm(formula, data = train_data, family = binomial)
  models[[paste(pair, collapse = " + ")]] <- model
  predictions[, i] <- predict(model, train_data, type = "response")
}

# Ensemble prediction using the mean of individual model predictions
ensemble_prediction <- na.omit(rowMeans(predictions))

# Convert probabilities to binary presence/absence using different thresholds
calculate_tss <- function(predicted, actual, threshold) {
  predicted_binary <- ifelse(predicted >= threshold, 1, 0)
  tp <- sum(predicted_binary == 1 & actual == 1)
  tn <- sum(predicted_binary == 0 & actual == 0)
  fp <- sum(predicted_binary == 1 & actual == 0)
  fn <- sum(predicted_binary == 0 & actual == 1)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  tss <- sensitivity + specificity - 1
  return(tss)
}

# Actual presence/absence for the training data
actual <- train_data$pa

# Calculate TSS for a range of thresholds
thresholds <- seq(0, 1, by = 0.01)
tss_values <- sapply(thresholds, function(threshold) calculate_tss(ensemble_prediction, actual, threshold))

# Find the optimal threshold and corresponding TSS
optimal_index <- which.max(tss_values)
optimal_threshold <- thresholds[optimal_index]
optimal_tss <- tss_values[optimal_index]

# Output results
cat("Optimal Threshold:", optimal_threshold, "\n")
cat("Maximum TSS:", optimal_tss, "\n")
# Export results to a CSV file
results_df <- data.frame(
  Optimal_Threshold = optimal_threshold,
  Maximum_TSS = optimal_tss
)
write.csv(results_df, file = "esm_optimal_thresholds_tss_11_40.csv", row.names = FALSE)

# Plot TSS values against thresholds
pdf("esm_TSS_vs_Threshold_plot_11_40.pdf")  # Open a PDF file to save the plot
plot(thresholds, tss_values, type = "l", col = "blue", lwd = 2,
     xlab = "Threshold", ylab = "TSS", main = "TSS vs Threshold")
abline(v = optimal_threshold, col = "red", lty = 2)
dev.off()  # Close the PDF file





#################################################################################################


set.seed(123)
# Extract predictor variables from the raster stack for presence points
xy <- maxent_data[, c("longitude", "latitude")]
cell_indices <- raster::extract(final_raster_stack_81_10, xy)
colnames(cell_indices) <- c(paste0("bio", 1:19), "slope", "SurficialGeology", "TWI", "aspect")
cell_indices <- data.frame(cell_indices)

# Calculate the correlation matrix
pair_matrix <- cor(na.omit(cell_indices))

# Initialize an empty list to store all pairs
all_pairs <- list()

# Iterate through each element in the correlation matrix
for (i in 1:(ncol(pair_matrix) - 1)) {
  for (j in (i + 1):ncol(pair_matrix)) {
    # Capture all pairs
    all_pairs[[length(all_pairs) + 1]] <- c(rownames(pair_matrix)[i], colnames(pair_matrix)[j])
  }
}

# Convert the list of all pairs to a data frame for display
all_pairs <- do.call(rbind, all_pairs)
colnames(all_pairs) <- c("Variable1", "Variable2")

# Withholding a 30% sample for testing
set.seed(123) # for reproducibility
fold <- kfold(maxent_data, k = 3)
occtest <- maxent_data[fold == 1, c("longitude", "latitude")]
occtrain <- maxent_data[fold != 1, c("longitude", "latitude")]

# Extract predictor variables for training data
# 3.3 Model evaluation To evaluate models, we use the evaluate function from the "dismo" package. 
coordinates(occtrain) <- ~longitude + latitude # replace 'longitude' and 'latitude' with actual column names
train_data_values <- extract(final_raster_stack_81_10, occtrain)
train_data <- data.frame(pa = 1, train_data_values)

# Add pseudo-absences
occtrain <- data.frame(na.omit(occtrain))
pseudo_absences <- spsample(as(extent(WYO_boundary), "SpatialPolygons"), n = nrow(occtrain), type = "random")
pseudo_absence_values <- extract(final_raster_stack_81_10, pseudo_absences)
pseudo_absence_data <- data.frame(pa = 0, pseudo_absence_values)

# Combine occurrence and pseudo-absence data
train_data <- rbind(train_data, pseudo_absence_data)

# Ensure column names are set correctly
colnames(train_data) <- c("pa", paste0("bio", 1:19), "slope", "SurficialGeology", "TWI", "aspect")

# Fit logistic regression models for each valid pair
models <- list()
predictions <- matrix(0, nrow = nrow(train_data), ncol = length(all_pairs))

for (i in seq_along(all_pairs)) {
  pair <- all_pairs[[i]]
  formula <- as.formula(paste("pa ~", paste(pair, collapse = " + ")))
  model <- glm(formula, data = train_data, family = binomial)
  models[[paste(pair, collapse = " + ")]] <- model
  predictions[, i] <- predict(model, train_data, type = "response")
}

# Ensemble prediction using the mean of individual model predictions
ensemble_prediction <- na.omit(rowMeans(predictions))

# Convert probabilities to binary presence/absence using different thresholds
calculate_tss <- function(predicted, actual, threshold) {
  predicted_binary <- ifelse(predicted >= threshold, 1, 0)
  tp <- sum(predicted_binary == 1 & actual == 1)
  tn <- sum(predicted_binary == 0 & actual == 0)
  fp <- sum(predicted_binary == 1 & actual == 0)
  fn <- sum(predicted_binary == 0 & actual == 1)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  tss <- sensitivity + specificity - 1
  return(tss)
}

# Actual presence/absence for the training data
actual <- train_data$pa

# Calculate TSS for a range of thresholds
thresholds <- seq(0, 1, by = 0.01)
tss_values <- sapply(thresholds, function(threshold) calculate_tss(ensemble_prediction, actual, threshold))

# Find the optimal threshold and corresponding TSS
optimal_index <- which.max(tss_values)
optimal_threshold <- thresholds[optimal_index]
optimal_tss <- tss_values[optimal_index]

# Output results
cat("Optimal Threshold:", optimal_threshold, "\n")
cat("Maximum TSS:", optimal_tss, "\n")
# Export results to a CSV file
results_df <- data.frame(
  Optimal_Threshold = optimal_threshold,
  Maximum_TSS = optimal_tss
)
write.csv(results_df, file = "esm_optimal_thresholds_tss_81_10.csv", row.names = FALSE)

# Plot TSS values against thresholds
pdf("esm_TSS_vs_Threshold_plot_81_10.pdf")  # Open a PDF file to save the plot
plot(thresholds, tss_values, type = "l", col = "blue", lwd = 2,
     xlab = "Threshold", ylab = "TSS", main = "TSS vs Threshold (1980-2010)")
abline(v = optimal_threshold, col = "red", lty = 2)
dev.off()  # Close the PDF file
