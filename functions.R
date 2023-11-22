print("Load functions")

################################################################################
#### Input data preprocessing ##################################################
################################################################################

remove_outlier <- function(data, column) {
  
  IQR <- quantile(data[, get(column)], 0.75) - quantile(data[, get(column)], 0.25) # IQR= Q3 (75% quantile) – Q1 (25% quantile)
  # Outlier would be a point below [Q1- (1.5)IQR] or above [Q3+(1.5)IQR]
  l <- quantile(data[, get(column)], 0.25) - 1.5*IQR
  r <- quantile(data[, get(column)], 0.75) + 1.5*IQR
  
  data <- data %>%
    .[get(column) >= l & get(column) <= r]
  
  return(data)
  
}

################################################################################
#### Parameter calibration #####################################################
################################################################################


print("Functions have been loaded")