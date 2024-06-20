
set.seed(3)
pa_41_70 <- rbind(as.data.frame(p_41_70), as.data.frame(a_41_70))
pa_41_70$pa <- as.factor(c(rep(1, nrow(p_41_70)), rep(0, nrow(a_41_70))))
colnames(pa_41_70) <- c(paste0("bio", 1:19), "slope", "SurficialGeology", "TWI", "aspect", "pa")
# Save the pa_41_70 data to a CSV file
write.csv(pa_41_70, "pa_41_70.csv", row.names = FALSE)
xdata <- pa_41_70[,1:23] #X is automatically standardized when input into the model!!!
ydata <- pa_41_70[,24]
ydata <- as.data.frame(ydata)
## transform aspect to a linear variable
xdata$aspect <- cos((xdata$aspect*pi)/180)
#extract scaling information 
(x.scaled <- scale(xdata[,c(1:20,22:23)],center=TRUE,scale=TRUE))
data.scaled_41_70 <- cbind(x.scaled, SurficialGeology = xdata$SurficialGeology, TWI = xdata$TWI, pa = ydata)

year_model_41_70 <- glm(ydata ~ bio14 + bio1 + slope
                        + SurficialGeology + TWI + aspect,
                        data = data.scaled_41_70, family = binomial(link="logit"))
summary(year_model_41_70)
vif(year_model_41_70)

seasonal_model_41_70 <- glm(ydata ~ bio15 + bio16 + 
                              bio4 + I(bio4^2) + TWI + slope 
                            + aspect + I(aspect^2),
                            data = data.scaled_41_70, family = binomial(link="logit"))
summary(seasonal_model_41_70)
vif(seasonal_model_41_70)

annual_model_41_70 <- glm(ydata ~ bio1 + I(bio1^2) + bio12 + bio7 + 
                            I(bio7^2) + TWI + slope + 
                            aspect + I(aspect^2),
                          data = data.scaled_41_70, family = binomial(link="logit"))
summary(annual_model_41_70)
vif(annual_model_41_70)

winter_model_41_70 <- glm(ydata ~ bio11 + I(bio11^2) + TWI + I(TWI^2) + slope 
                          + I(slope^2) + aspect + I(aspect^2),
                          data = data.scaled_41_70, family = binomial(link="logit"))
summary(winter_model_41_70)
vif(winter_model_41_70)

drought_model_41_70 <- glm(ydata ~ bio10 + bio13 + bio14 
                           + TWI + slope + aspect + I(aspect^2),
                           data = data.scaled_41_70, family = binomial(link="logit"))
summary(drought_model_41_70)
vif(drought_model_41_70)

cold_model_41_70 <- glm(ydata ~ bio13 + bio14 + bio6 + I(bio6^2) 
                        + bio7 + I(bio7^2) + TWI + slope + aspect + I(aspect^2),
                        data = data.scaled_41_70, family = binomial(link="logit"))
summary(cold_model_41_70)
vif(cold_model_41_70)

extremes_model_41_70 <- glm(ydata ~ bio13 + bio17 + bio5 + I(bio5^2) 
                            + bio7 + I(bio7^2) + TWI + slope + aspect + I(aspect^2),
                            data = data.scaled_41_70, family = binomial(link="logit"))
summary(extremes_model_41_70)
vif(extremes_model_41_70)

# TSS Calculations for GLM models

glm_models <- list(
  year_model_41_70 = year_model_41_70,
  seasonal_model_41_70 = seasonal_model_41_70,
  annual_model_41_70 = annual_model_41_70,
  winter_model_41_70 = winter_model_41_70,
  drought_model_41_70 = drought_model_41_70,
  cold_model_41_70 = cold_model_41_70,
  extremes_model_41_70 = extremes_model_41_70
)

glm_models <- list(
  "Year Model (2041-2070)" = year_model_41_70,
  "Seasonal Model (2041-2070)" = seasonal_model_41_70,
  "Annual Model (2041-2070)" = annual_model_41_70,
  "Winter Model (2041-2070)" = winter_model_41_70,
  "Drought Model (2041-2070)" = drought_model_41_70,
  "Cold Model (2041-2070)" = cold_model_41_70,
  "Extremes Model (2041-2070)" = extremes_model_41_70
)

# Function to calculate TSS for a given model and threshold
calculate_tss <- function(model, data, threshold) {
  predictions <- predict(model, data, type = "response")
  actual <- data$ydata
  
  # Remove rows with NA in either predictions or actual
  na_index <- which(is.na(predictions) | is.na(actual))
  predictions <- predictions[-na_index]
  actual <- actual[-na_index]
  
  predicted_classes <- ifelse(predictions > threshold, 1, 0)
  TP <- sum(predicted_classes == 1 & actual == 1)
  TN <- sum(predicted_classes == 0 & actual == 0)
  FP <- sum(predicted_classes == 1 & actual == 0)
  FN <- sum(predicted_classes == 0 & actual == 1)
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  # Handle edge cases where sensitivity or specificity might be NaN
  sensitivity <- ifelse(is.nan(sensitivity), 0, sensitivity)
  specificity <- ifelse(is.nan(specificity), 0, specificity)
  
  TSS <- sensitivity + specificity - 1
  
  return(TSS)
}


# Calculate TSS and find optimal threshold for each model
results <- lapply(glm_models, function(model) {
  predictions <- predict(model, data.scaled_41_70, type = "response")
  actual <- data.scaled_41_70$ydata
  
  thresholds <- seq(0, 1, by = 0.01)
  tss_values <- sapply(thresholds, function(threshold) calculate_tss(model, data.scaled_41_70, threshold))
  
  optimal_index <- which.max(tss_values)
  optimal_threshold <- thresholds[optimal_index]
  optimal_tss <- tss_values[optimal_index]
  
  return(list(
    model = model,
    optimal_threshold = optimal_threshold,
    optimal_tss = optimal_tss,
    tss_values = tss_values
  ))
})

# Print and analyze results
for (i in seq_along(results)) {
  cat("Model:", names(glm_models)[i], "\n")
  cat("Optimal Threshold:", results[[i]]$optimal_threshold, "\n")
  cat("Maximum TSS:", results[[i]]$optimal_tss, "\n\n")
}

# Print and export results
results_df <- data.frame(
  Model = names(glm_models),
  Optimal_Threshold = sapply(results, function(res) res$optimal_threshold),
  Maximum_TSS = sapply(results, function(res) res$optimal_tss)
)

print(results_df)

write.csv(results_df, "glm_tss_results_41_70.csv", row.names = FALSE)

# Plot TSS values against thresholds for each model and save plots
pdf("glm_tss_plots_41_70.pdf")

# Plot TSS values against thresholds for each model
par(mfrow = c(4, 2))  # Adjust layout for 7 models

for (i in seq_along(results)) {
  plot(thresholds, results[[i]]$tss_values, type = "l", col = "blue", lwd = 2,
       xlab = "Threshold", ylab = "TSS", main = paste("TSS vs Threshold -", names(glm_models)[i]))
  abline(v = results[[i]]$optimal_threshold, col = "red", lty = 2)
}
dev.off()
par(mfrow = c(1, 1))  # Readjust layout

# Export GLM summaries to a text file
output_summary <- capture.output(
  for (model_name in names(glm_models)) {
    cat("Model:", model_name, "\n")
    print(summary(glm_models[[model_name]]))
    cat("\n\n")
  }
)

writeLines(output_summary, "glm_model_summaries_41_70.txt")



####################################################################################


# Function to calculate TSS and find optimal threshold
calculate_TSS_with_threshold <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, data, type = "response")
  
  # Combine predicted probabilities with actual y-values
  df <- data.frame(prob = predicted_probs, y = data$ydata)
  
  # Sort by predicted probability in descending order
  df <- df[order(-df$prob), ]
  
  # Initialize variables for TSS calculation
  best_tss <- -Inf
  best_threshold <- NA
  
  # Loop over each unique predicted probability as a potential threshold
  for (i in 1:nrow(df)) {
    threshold <- df$prob[i]
    
    # Predicted classes based on threshold
    y_pred <- ifelse(predicted_probs >= threshold, 1, 0)
    
    # Calculate Sensitivity and Specificity
    TP <- sum(y_pred == 1 & data$ydata == 1)
    TN <- sum(y_pred == 0 & data$ydata == 0)
    FP <- sum(y_pred == 1 & data$ydata == 0)
    FN <- sum(y_pred == 0 & data$ydata == 1)
    
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    
    # Calculate TSS
    current_tss <- sensitivity + specificity - 1
    
    # Update best TSS and threshold if current TSS is higher
    if (current_tss > best_tss) {
      best_tss <- current_tss
      best_threshold <- threshold
    }
  }
  
  return(list(TSS = best_tss, OptimalThreshold = best_threshold))
}

# Calculate TSS and optimal threshold for each model
tss_threshold_results <- lapply(glm_models, function(model) {
  calculate_TSS_with_threshold(model, data.scaled_81_10)
})

# Print TSS and optimal threshold for each model
for (i in seq_along(tss_threshold_results)) {
  model_name <- names(glm_models)[i]
  tss <- tss_threshold_results[[i]]$TSS
  threshold <- tss_threshold_results[[i]]$OptimalThreshold
  cat("Model:", model_name, "\n")
  cat("TSS:", tss, "\n")
  cat("Optimal Threshold:", threshold, "\n\n")
}





par(mfrow=c(2,2))
# Define a function to calculate TSS and find optimal threshold
calculate_TSS_with_threshold <- function(model, data) {
  # Predicted probabilities
  predicted_probs <- predict(model, data, type = "response")
  
  # Combine predicted probabilities with actual y-values
  df <- data.frame(prob = predicted_probs, y = data$ydata)
  
  # Sort by predicted probability in descending order
  df <- df[order(-df$prob), ]
  
  # Initialize variables for TSS calculation
  best_tss <- -Inf
  best_threshold <- NA
  
  # Loop over each unique predicted probability as a potential threshold
  for (i in 1:nrow(df)) {
    threshold <- df$prob[i]
    
    # Predicted classes based on threshold
    y_pred <- ifelse(predicted_probs >= threshold, 1, 0)
    
    # Calculate Sensitivity and Specificity
    TP <- sum(y_pred == 1 & data$ydata == 1)
    TN <- sum(y_pred == 0 & data$ydata == 0)
    FP <- sum(y_pred == 1 & data$ydata == 0)
    FN <- sum(y_pred == 0 & data$ydata == 1)
    
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    
    # Calculate TSS
    current_tss <- sensitivity + specificity - 1
    
    # Update best TSS and threshold if current TSS is higher
    if (current_tss > best_tss) {
      best_tss <- current_tss
      best_threshold <- threshold
    }
  }
  
  return(list(TSS = best_tss, OptimalThreshold = best_threshold))
}

# Calculate TSS and optimal threshold for each model
results <- list()
thresholds <- seq(0, 1, by = 0.01)  # Define thresholds from 0 to 1 with step 0.01

for (i in seq_along(glm_models)) {
  model_name <- names(glm_models)[i]
  model <- glm_models[[model_name]]
  
  # Initialize TSS values storage
  tss_values <- numeric(length(thresholds))
  
  # Loop over thresholds
  for (j in seq_along(thresholds)) {
    threshold <- thresholds[j]
    
    # Predicted classes based on current threshold
    y_pred <- ifelse(predict(model, data.scaled_81_10, type = "response") >= threshold, 1, 0)
    
    # Calculate Sensitivity and Specificity
    TP <- sum(y_pred == 1 & data.scaled_81_10$ydata == 1)
    TN <- sum(y_pred == 0 & data.scaled_81_10$ydata == 0)
    FP <- sum(y_pred == 1 & data.scaled_81_10$ydata == 0)
    FN <- sum(y_pred == 0 & data.scaled_81_10$ydata == 1)
    
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    
    # Calculate TSS
    tss_values[j] <- sensitivity + specificity - 1
  }
  
  # Determine optimal threshold and corresponding TSS
  optimal_threshold <- thresholds[which.max(tss_values)]
  optimal_tss <- max(tss_values)
  
  # Store results
  results[[model_name]] <- list(tss_values = tss_values, optimal_threshold = optimal_threshold)
  
  # Plot TSS vs Threshold
  plot(thresholds, tss_values, type = "l", col = "blue", lwd = 2,
       xlab = "Threshold", ylab = "TSS", main = paste("GLM TSS vs Threshold -", model_name))
  abline(v = optimal_threshold, col = "red", lty = 2)
}

################ first 4 

# Calculate TSS and optimal threshold for each model
results <- list()##
thresholds <- seq(0, 1, by = 0.01)  # Define thresholds from 0 to 1 with step 0.01

# Loop over each model, up to the first four
for (i in seq_along(glm_models)) {
  if (i > 4) break  # Exit the loop after the fourth model
  
  model_name <- names(glm_models)[i]
  model <- glm_models[[model_name]]
  
  # Initialize TSS values storage
  tss_values <- numeric(length(thresholds))
  
  # Loop over thresholds
  for (j in seq_along(thresholds)) {
    threshold <- thresholds[j]
    
    # Predicted classes based on current threshold
    y_pred <- ifelse(predict(model, data.scaled_81_10, type = "response") >= threshold, 1, 0)
    
    # Calculate Sensitivity and Specificity
    TP <- sum(y_pred == 1 & data.scaled_81_10$ydata == 1)
    TN <- sum(y_pred == 0 & data.scaled_81_10$ydata == 0)
    FP <- sum(y_pred == 1 & data.scaled_81_10$ydata == 0)
    FN <- sum(y_pred == 0 & data.scaled_81_10$ydata == 1)
    
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    
    # Calculate TSS
    tss_values[j] <- sensitivity + specificity - 1
  }
  
  # Determine optimal threshold and corresponding TSS
  optimal_threshold <- thresholds[which.max(tss_values)]
  optimal_tss <- max(tss_values)
  
  # Store results
  results[[model_name]] <- list(tss_values = tss_values, optimal_threshold = optimal_threshold)
  
  # Plot TSS vs Threshold
  plot(thresholds, tss_values, type = "l", col = "blue", lwd = 2,
       xlab = "Threshold", ylab = "TSS", main = paste("GLM TSS vs Threshold -", model_name))
  abline(v = optimal_threshold, col = "red", lty = 2)
}


