remove_outliers_iqr <- function(data) {
  IQR_value <- IQR(data)
  Q1 <- quantile(data, 0.25)
  Q3 <- quantile(data, 0.75)
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  filtered_data <- data[data > lower_bound & data < upper_bound]
  
  return(filtered_data)
}

remove_outliers_z_score <- function(data, threshold = 3) {
  z_scores <- (data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE)
  filtered_data <- data[abs(z_scores) < threshold]
  return(filtered_data)
}

remove_outliers_modified_z_score <- function(data, threshold = 3.5) {
  median_val <- median(data)
  mad_val <- mad(data, constant = 1) # MAD con constante para emular el SD
  
  modified_z_scores <- 0.6745 * (data - median_val) / mad_val
  filtered_data <- data[abs(modified_z_scores) < threshold]
  return(filtered_data)
}