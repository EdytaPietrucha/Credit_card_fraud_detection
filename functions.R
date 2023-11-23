print("Load functions")

################################################################################
#### Input data preprocessing ##################################################
################################################################################

# Remove outliers from dataset -------------------------------------------------
remove_outlier <- function(data, column) {
  
  IQR <- quantile(data[, get(column)], 0.75) - quantile(data[, get(column)], 0.25) # IQR= Q3 (75% quantile) – Q1 (25% quantile)
  # Outlier would be a point below [Q1- (1.5)IQR] or above [Q3+(1.5)IQR]
  l <- quantile(data[, get(column)], 0.25) - 1.5*IQR
  r <- quantile(data[, get(column)], 0.75) + 1.5*IQR
  
  data <- data %>%
    .[get(column) >= l & get(column) <= r]
  
  return(data)
  
}

# Data preprocessing used for test dataset (logistic regression) ---------------
data_preprocess_logreg <- function(data) {
  
  # Select useful columns
  data <- data %>%
    .[,c("is_fraud", "category", "amt", "trans_date_trans_time", "dob")]
  
  # Convert into factors
  dt <- c( "category", "is_fraud")
  data[,(dt):= lapply(.SD, as.factor), .SDcols = dt]
  
  # Convert into data class
  data[, trans_date_trans_time := strptime(trans_date_trans_time,
                                                 format ="%Y-%m-%d %H:%M:%OS", 
                                                 tz = "EST")]
  # Compute age of the customer in a day of transaction
  data[, age := round(as.numeric(difftime(trans_date_trans_time, dob, units = "days")/365), 0)]
  # Adding a weekday of transaction
  data[, week_day_of_trans := as.factor(wday(trans_date_trans_time))]
  # Hour of the transaction
  data[, trans_hour := as_factor(strftime(trans_date_trans_time, 
                                                format="%H", 
                                                tz = "EST"))]
  # Adding bin variables
  data[, weekday_fraud := if_else(week_day_of_trans %in% 4:7, 1, 0)]
  data[, category_fraud := 
               if_else(category %in% c('grocery_pos', 'misc_net', 'shopping_net', 'shopping_pos'), 1, 0)]
  data[, hour_fraud := 
               if_else(trans_hour %in% c('00', '01', '02', '03', '22', '23'), 1, 0)]
  data[,c("weekday_fraud", "category_fraud", "hour_fraud"):= 
               lapply(.SD, as.factor), .SDcols = c("weekday_fraud", "category_fraud", "hour_fraud")]
  
  # normalization
  temp <- c('amt','age')
  data[, (temp) := lapply(.SD, min_max_norm), .SDcols = temp]
  
  return(data)
  
}

# Data preprocessing used for test dataset (random forest) ---------------------
data_preprocess_forest <- function(data) {
  
  # Select useful columns
  data <- data %>%
    .[,c("is_fraud", "category", "amt", "trans_date_trans_time", "dob")]
  
  # Convert into factors
  dt <- c( "category", "is_fraud")
  data[,(dt):= lapply(.SD, as.factor), .SDcols = dt]
  
  # Convert into data class
  data[, trans_date_trans_time := strptime(trans_date_trans_time,
                                           format ="%Y-%m-%d %H:%M:%OS", 
                                           tz = "EST")]
  # Compute age of the customer in a day of transaction
  data[, age := round(as.numeric(difftime(trans_date_trans_time, dob, units = "days")/365), 0)]
  # Adding a weekday of transaction
  data[, week_day_of_trans := as.factor(wday(trans_date_trans_time))]
  # Hour of the transaction
  data[, trans_hour := as_factor(strftime(trans_date_trans_time, 
                                          format="%H", 
                                          tz = "EST"))]
  # Adding bin variables
  data[, weekday_fraud := if_else(week_day_of_trans %in% 4:7, 1, 0)]
  data[, category_fraud := 
         if_else(category %in% c('grocery_pos', 'misc_net', 'shopping_net', 'shopping_pos'), 1, 0)]
  data[, hour_fraud := 
         if_else(trans_hour %in% c('00', '01', '02', '03', '22', '23'), 1, 0)]
  data[,c("weekday_fraud", "category_fraud", "hour_fraud"):= 
         lapply(.SD, as.factor), .SDcols = c("weekday_fraud", "category_fraud", "hour_fraud")]
  
  return(data)
  
}

################################################################################
#### Data visualization ########################################################
################################################################################

# Custom figures style ---------------------------------------------------------
text_size <- 12
# customize my_style settings
my_style <-  theme_light() + 
  theme(
    legend.position = 'none',
    plot.subtitle = element_text(size = text_size - 4, colour='black'),
    plot.title = element_text(size = text_size, face = 'bold'),
    axis.text=element_text(size=text_size - 4), 
    axis.title.y = element_text(size = text_size - 4), 
    axis.title.x = element_text(size = text_size - 4),
    legend.background = element_rect(fill = "white")
  ) 
# define my palette colors
my_palette <- c("#D73027", "#FDAE61", "#FFFFBF", 
                         "#A50026", '#ABD9E9', '#4575B4', 
                         '#313695', 'black', "lawngreen")
                         
################################################################################
#### Parameter calibration #####################################################
################################################################################

# Confusion matrix -------------------------------------------------------------
confusion_matrix <- function(model, data, col, cutoff=0.5, model_type = "logistic") {
  
  predictScore <- as.numeric(predict(object=model, type='response',newdat=data)) # calculating prediction scores
  
  if (model_type == "forest") predictScore = predictScore -1
  
  predic <- if_else(predictScore > cutoff, 1, 0) # assigning predictions
  confmat <- table(predic, data[[col]])
  
  rownames(confmat) <- c('pred_negative', 'pred_positive')
  colnames(confmat) <- c('obs_negative', 'obs_positive')
  
  print(confmat) # printing the confusion matrix
  
  precision <- confmat[2,2]/(confmat[2,1]+confmat[2,2]) # precision = TP/(TP+FP)
  recall <- confmat[2,2]/(confmat[1,2]+confmat[2,2]) # recall = TP/(TP+FN)
  
  print(paste('Precision: ',precision))
  print(paste('Recall: ', recall))
  
  return(c(precision, recall))
}

# The most reasonable cutoff for a logistic model (plot) -----------------------
find_cutoff <- function(model, data, target_variable, min_cutoff=0.1, max_cutoff=0.8, by=0.05) {
  
  cutoffs <- seq(min_cutoff, max_cutoff, by=by)
  precisions <- numeric(0)
  recalls <- numeric(0)
  
  for(x in cutoffs){
    # confusion matrix for the model with cutoff = x
    conf_mat <- confusion_matrix(model, data, target_variable, x)
    precisions <- c(precisions, conf_mat[1])
    recalls <- c(recalls, conf_mat[2])
  }
  # preparation for a plot
  metrics <- data.table(
    cutoffs = cutoffs,
    precisions = precisions,
    recalls = recalls
  )
  
  metrics_long <- melt(metrics, id='cutoffs')
  
  pl <- ggplot(data=metrics_long, aes(x=cutoffs, y=value, colour=variable)) +
    geom_line(size=1) +
    scale_colour_discrete(name ="Metric",
                          breaks=c("precisions", "recalls"),
                          labels=c("Precision", "Recall")) +
    labs(title='Precision & recall rates',
         subtitle='as functions of the cutoff',
         x='Cutoff', y='') +
    my_style +
    theme(legend.position = c(0.9,0.9), axis.title.y = element_blank()) 
  
  return(pl)
}

# Best parameters for classwt for random forest --------------------------------
find_classwt <- function(data) {
  
  recalls <- numeric(0)
  precisions <- numeric(0)
  
  for(t in seq(0.05, 0.95, by=0.05)){
    gc() # freeing unused memory
    # fitting random forest with parameter classwt = t for class '0' and = 1-t for '1'
    forest <- randomForest(as_factor(is_fraud) ~ amt + age + category_fraud + hour_fraud,
                           data=data, ntree=100, classwt = c('0' = t, '1' = 1-t))
    # confusion matrix for the model
    conf_mat <- confusion_matrix_rf(forest, data, 'is_fraud')
    recalls <- c(recalls, conf_mat[2])
    precisions <- c(precisions, conf_mat[1])
  }
  # preparation for plotting
  metrics <- tibble(
    weights = seq(0.05, 0.95, by=0.05),
    precisions = precisions,
    recalls = recalls
  )
  metrics_long <- melt(metrics, id='weights')
  pl <- ggplot(data=metrics_long, aes(x=weights, y=value, colour=variable)) +
    geom_line(size=1) +
    scale_colour_discrete(name ="Metric",
                          breaks=c("precisions", "recalls"),
                          labels=c("Precision", "Recall")) +
    labs(title='Precision & recall rates',
         subtitle='as functions of the class weight (weight of the "0" class)',
         x='weight', y='') +
    theme_classic() +
    theme(legend.position = c(0.9,0.9), axis.title.y = element_blank()) +
    theme(plot.subtitle = element_text(size = 9, colour='darkgrey'))
  
  return(pl)
}

# Cross-validation density of metrics ------------------------------------------
cv_metrics_plt <- function(metrics, model_nm = "logistic regression") {
  
  metric_nm <- names(metrics)[2]
  title_nm <- paste0("Density plot for ", metric_nm)
  subtitle_nm <- paste0("for ", model_nm)
  
  plt <- ggplot(as.data.frame(metrics), aes(get(metric_nm), colour = model, fill = model)) +
    geom_density(alpha = 0.7) +
    theme_light() +
    theme(
      plot.subtitle = element_text(size = text_size - 4, colour='black'),
      plot.title = element_text(size = text_size, face = 'bold'),
      axis.text=element_text(size=text_size - 4), 
      axis.title.y = element_text(size = text_size - 4), 
      axis.title.x = element_text(size = text_size - 4)
    ) +
    scale_fill_manual(values = c(my_palette[c(4,7)]))  + 
    labs(title = title_nm,
         subtitle = subtitle_nm,
         y = "",
         x = metric_nm)
  
  return(plt)
}

print("Functions have been loaded")