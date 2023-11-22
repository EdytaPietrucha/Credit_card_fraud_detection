# dev.off()
main_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(main_path)

source("libraries.R")
source("functions.R")

#### Source data  --------------------------------------------------------------
data_train <- fread(paste0(main_path,"/Data/fraudTrain.csv"))
dim(data_train)

#### Data preprocessing  -------------------------------------------------------
# Delete unuseful columns
data_train <- data_train %>%
  .[,-c("V1", "first", "last", "street", "zip", "unix_time", "trans_num")]

# Correct variables types
data_train %>%
  str

# Convert into factors
dt <- c("cc_num", "merchant", "category", "gender", "city", "state", "job", "is_fraud")
data_train[,(dt):= lapply(.SD, as.factor), .SDcols = dt]
# Convert into data class
data_train[, dob := as.Date(dob)]
data_train[, trans_date_trans_time := strptime(trans_date_trans_time,
                                               format ="%Y-%m-%d %H:%M:%OS", 
                                               tz = "EST")]

# Compute age of the customer ina day of transaction
data_train[, age := round(as.numeric(difftime(trans_date_trans_time, dob, units = "days")/365), 0)]


text_size <- 20
# customize my_style settings
my_style <-  theme_light() + 
  theme(
    legend.position = 'none',
    plot.subtitle = element_text(size = text_size - 8, colour='black'),
    plot.title = element_text(size = text_size, face = 'bold'),
    axis.text=element_text(size=text_size - 4), 
    axis.title.y = element_text(size = text_size - 4), 
    axis.title.x = element_text(size = text_size - 4),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color=NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
  ) 
# define my palette colors
my_palette <- c("#D73027", "#FDAE61", "#FFFFBF", 
                         "#A50026", '#ABD9E9', '#4575B4', 
                         '#313695', 'black', "lawngreen")
                         
ggplot(data_train, aes(x = is_fraud, y = age)) +
  geom_boxplot() +
  my_style +
  labs(title = "Boxplot for age of Customer",
       subtitle="group by not-fraudulent transactions and fraudulent transactions")
  
# Frequency of fraudulant transaction group by gender

gender_fraud <- data_train %>% 
  .[, .(count = .N), by = c("gender", "is_fraud")] %>%
  .[, .(is_fraud, freq = count/sum(count)), by = gender] %>%
  .[is_fraud == 1]

ggplot(gender_fraud, aes(x = gender, y = freq, fill = gender)) +
  geom_bar(stat = 'identity') +
  my_style +
  scale_fill_manual(values = c(my_palette[c(4,7)])) +
  labs(title = "Frequency of fraud group by gender",
       x = "",
       y = "")
  
# Amount of money spent on the transaction
ggplot(data_train, aes(x = is_fraud, y = amt)) +
  geom_boxplot() +
  my_style
# as we have plenty of outliers the figure is unreadable, lets remove the outliers and make a plot

remove_outlier <- function(data, column) {
  
  IQR <- quantile(data[, get(column)], 0.75) - quantile(data[, get(column)], 0.25) # IQR= Q3 (75% quantile) – Q1 (25% quantile)
  # Outlier would be a point below [Q1- (1.5)IQR] or above [Q3+(1.5)IQR]
  l <- quantile(data[, get(column)], 0.25) - 1.5*IQR
  r <- quantile(data[, get(column)], 0.75) + 1.5*IQR
  
  data <- data %>%
    .[get(column) >= l & get(column) <= r]
  
  return(data)
  
}

ggplot(remove_outlier(data_train, "amt"), 
       aes(x = is_fraud, y = amt)) +
  geom_boxplot() +
  my_style +
  labs(title = "Boxplot for amount spent on the transaction",
       subtitle = "group by not-fraudulent transactions and fraudulent transactions")

# Adding a weekday of transaction
data_train[, week_day_of_trans := as.factor(wday(trans_date_trans_time))]
# levels(data_train$week_day_of_trans) <- 

week_day_fraud <- data_train %>% 
  .[, .(count = .N), by = c("week_day_of_trans", "is_fraud")] %>%
  .[, .(is_fraud, freq = count/sum(count)), by = week_day_of_trans] %>%
  .[is_fraud == 1]

ggplot(week_day_fraud, aes(x = week_day_of_trans, y = freq, fill = week_day_of_trans)) +
  geom_bar(stat = 'identity') +
  my_style +
  scale_fill_manual(values = c(my_palette[1:7])) +
  labs(title = "Frequency of fraud group by weekday of the transaction",
       x = "",
       y = "") +
  scale_x_discrete(labels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday"))

# Category
category_fraud <- data_train %>% 
  .[, .(count = .N), by = c("category", "is_fraud")] %>%
  .[, .(is_fraud, freq = count/sum(count)), by = category] %>%
  .[is_fraud == 1]

ggplot(category_fraud, aes(x = category, y = freq, fill = category)) +
  geom_bar(stat = 'identity') +
  my_style +
  labs(title = "Frequency of fraud group by category of merchant",
       x = "",
       y = "") +
  coord_flip()

# Hour of the transaction
data_train[, trans_hour := as_factor(strftime(trans_date_trans_time, 
                                              format="%H", 
                                              tz = "EST"))]

hour_fraud <- data_train %>%
  .[, .(count = .N), by = c("trans_hour", "is_fraud")] %>%
  .[, .(is_fraud, freq = count/sum(count)), by = trans_hour] %>%
  .[is_fraud == 1]

ggplot(hour_fraud, aes(x = trans_hour, y = freq, fill = trans_hour)) +
  geom_bar(stat = 'identity') +
  my_style +
  labs(title = "Frequency of fraud group by transaction hour",
       x = "",
       y = "") +
  coord_flip()

# distance between the home of a Holder and the place of a Merchant
data_train[, dist := sqrt((long-merch_long)^2 + (lat-merch_lat)^2)]

ggplot(data_train, aes(x = is_fraud, y = dist)) +
  geom_boxplot() +
  my_style +
  labs(title = "Boxplot for distance between the home of a Holder and the place of a Merchant",
       subtitle = "group by not-fraudulent transactions and fraudulent transactions")

# Information Value

categorical_cols <- c()
for (col in colnames(data_train %>% .[, -c("cc_num", "is_fraud")])) {
  
  if(is.factor(data_train[[col]])) {
    categorical_cols <- c(categorical_cols, col)
  }
  
}

woe_table <- woe(data_train[, ..categorical_cols], 
                 as.factor(data_train$is_fraud),
                 zeroadj = 1)

# Adding bin variables

data_train[, weekday_fraud := if_else(week_day_of_trans %in% 4:7, 1, 0)]
data_train[, category_fraud := 
             if_else(category %in% c('grocery_pos', 'misc_net', 'shopping_net', 'shopping_pos'), 1, 0)]
data_train[, hour_fraud := 
             if_else(trans_hour %in% c('00', '01', '02', '03', '22', '23'), 1, 0)]
data_train[,c("weekday_fraud", "category_fraud", "hour_fraud"):= 
             lapply(.SD, as.factor), .SDcols = c("weekday_fraud", "category_fraud", "hour_fraud")]

# Normalization
min_max_norm <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

temp <- c('amt','city_pop','age','dist')
data_train_bin <- data_train
data_train_bin <- data_train_bin[, ..temp] %>% 
  apply(2, function(x) min_max_norm(x)) %>%
  as.data.table() %>% 
  cbind(data_train_bin %>% .[, -..temp])
# data_train_bin[, (temp) := lapply(.SD, min_max_norm), .SDcols = temp] # reference!

# Correlation
cor(data_train[, ..temp])
ggcorr(data_train[, ..temp], label = T)
corrplot::corrplot(cor(data_train[, ..temp]), method = 'number')

# Fitting logistic regression 
data_train_bin_cv <- resample_partition(data_train_bin, p = c(valid = 0.3, train = 0.7))
train_bin_cv <- data_train_bin[data_train_bin_cv$train$idx,]
valid_bin_cv <- data_train_bin[data_train_bin_cv$valid$idx,]

logreg <- glm(is_fraud ~ 
                category_fraud + amt + hour_fraud + age:hour_fraud + amt:hour_fraud + amt:category_fraud + age:category_fraud,
               data = train_bin_cv, family = binomial)
summary(logreg)

# a function computing confusion matrix - based on lecture materials
confusion_matrix <- function(model, data, col, cutoff=0.5) {
  
  predictScore <- predict(object=model, type='response',newdat=data) # calculating prediction scores
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

confusion_matrix_rf <- function(model, data, col, cutoff=0.5){
  predictScore <- as.numeric(predict(object=model, type='response',newdat=data)) -1
  predic <- ifelse(predictScore > cutoff, 1, 0)
  confmat <- table(predic, data[[col]])
  rownames(confmat) <- c('pred_negative', 'pred_positive')
  colnames(confmat) <- c('obs_negative', 'obs_positive')
  print(confmat)
  precision <- confmat[2,2]/(confmat[2,1]+confmat[2,2])
  recall <- confmat[2,2]/(confmat[1,2]+confmat[2,2])
  print(paste('Precision: ',precision))
  print(paste('Recall: ', recall))
  return(c(precision, recall))
}

confusion_matrix(logreg, train_bin_cv, col = "is_fraud", cutoff = 0.003)
confusion_matrix(logreg, valid_bin_cv, col = "is_fraud", cutoff = 0.003)

# looking for the most reasonable cutoff for a model - the function produces a plot
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

cutoff_lvl <- find_cutoff(logreg, train_bin_cv, 'is_fraud', 0.001, 0.1, 0.001)

# Logistic regression with PCA

# remove dependent variable from dataset and keep only numerical variables
numerical_cols <- c('amt', 'city_pop', 'age', 'dist', 'weekday_fraud', 'category_fraud', 'hour_fraud')
data_bin_PCA <- data_train_bin %>% 
  .[, (numerical_cols):= lapply(.SD, as.numeric), .SDcols = numerical_cols] %>%
  .[, ..numerical_cols]

# the PCA analysis:
pca_decomposition <- prcomp(data_bin_PCA)

# calculating a variance of each component
variance <- pca_decomposition$sdev^2

# calculating a percentage of variance explained by each component
variance_perc <- round(variance/sum(variance),4)

ggplot(data = as.data.frame(variance_perc), 
       aes(x = paste0('PC', 1:7), y = variance_perc)) +
  geom_col() +
  xlab("") + 
  labs(title= 'Variance', subtitle='for each PCs',x='', y='') +
  my_style

# create the datasets to be used for the modelling
data_train_pca <- cbind(data_train$is_fraud, as.data.table(pca_decomposition$x)) %>%
  setNames(c("is_fraud", paste0("PC", 1:7)))
train_bin_cv_pca <- data_train_pca[data_train_bin_cv$train$idx,]
valid_bin_cv_pca <- data_train_pca[data_train_bin_cv$valid$idx,]

logreg_PCA <- glm(is_fraud ~ 
                    PC1 + PC2 + PC3 + PC4 + PC2*PC3 + PC1*PC4,
                  data = train_bin_cv_pca, family = binomial)
summary(logreg_PCA)
  
cutoff_lvl_pca <- find_cutoff(logreg_PCA, train_bin_cv_pca, 'is_fraud', 0.001, 0.05, 0.001)

confusion_matrix(logreg_PCA, train_bin_cv_pca, col = "is_fraud", cutoff = 0.003)
confusion_matrix(logreg_PCA, valid_bin_cv_pca, col = "is_fraud", cutoff = 0.003)

# Random forest
# CV on unnormalized data
train_cv <- data_train[data_train_bin_cv$train$idx,]
valid_cv <- data_train[data_train_bin_cv$valid$idx,]

# Searching best parameters for classwt
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

set.seed(123)
subset_tr <- sample_n(train_cv, 10^5)
rf_params <- find_classwt(subset_tr)

forest <- randomForest(as_factor(is_fraud) ~ amt + age + category_fraud + hour_fraud,
                       data=train_cv, 
                       ntree=100, 
                       classwt = c('0' = 0.25, '1' = 0.75))

confusion_matrix_rf(forest, train_cv, 'is_fraud')
confusion_matrix_rf(forest, valid_cv, 'is_fraud')

#### Cross validation - Logistic regression ------------------------------------

# To check performance of the models, we sample a subset of the data, containing 1e5 observations
set.seed(125)
n_obs <- 1e5
subset <- sample_n(data_train_bin[,c("is_fraud", "amt", "age", "category_fraud", "hour_fraud")], n_obs) # subset for the analysis
test_ratio <- 0.2
N <- 200

# Creating train/test sets
cv_mc <- crossv_mc(subset, N, test=test_ratio)

# fitting models on the training sets
models <- map(cv_mc$train, ~ glm(is_fraud ~ category_fraud + amt + hour_fraud + 
                                   age:hour_fraud + amt:hour_fraud + amt:category_fraud + age:category_fraud,
                                 data=., family=binomial))

recalls_train <- map2_dbl(models, cv_mc$train,
                          ~confusion_matrix(model=.x, data=as_tibble(.y), col='is_fraud',
                                            cutoff=0.003)[2])

precisions_train <- map2_dbl(models, cv_mc$train,
                             ~confusion_matrix(model=.x, data=as_tibble(.y), col='is_fraud',
                                               cutoff=0.003)[1])

recalls_test <- map2_dbl(models, cv_mc$test,
                         ~confusion_matrix(model=.x, data=as_tibble(.y), col='is_fraud',
                                           cutoff=0.003)[2])

precisions_test <- map2_dbl(models, cv_mc$test,
                            ~confusion_matrix(model=.x, data=as_tibble(.y), col='is_fraud',
                                              cutoff=0.003)[1])

recalls_all <- rbind(data.table(model = 'train_data', 
                                recalls = recalls_train),
                     data.table(model = 'test_data', 
                                recalls = recalls_test))

precisions_all <- rbind(data.table(model = 'train_data', 
                                   precisions = precisions_train),
                        data.table(model = 'test_data', 
                                   precisions = precisions_test))

# Density plot of metrics
cv_metrics_plt <- function(metrics, model_nm = "logistic regression") {
  
  metric_nm <- names(metrics)[2]
  title_nm <- paste0("Density plot for ", metric_nm)
  subtitle_nm <- paste("for ", model_nm)
  
  plt <- ggplot(as.data.frame(metrics), aes(get(metric_nm), colour = model, fill = model)) +
    geom_density(alpha = 0.7) +
    theme_light() +
    theme(
      plot.subtitle = element_text(size = text_size - 8, colour='black'),
      plot.title = element_text(size = text_size, face = 'bold'),
      axis.text=element_text(size=text_size - 4), 
      axis.title.y = element_text(size = text_size - 4), 
      axis.title.x = element_text(size = text_size - 4)
    ) +
    scale_fill_manual(values = c(my_palette[c(4,7)]))  + 
    labs(title = title_nm,
         subtitle = subtitle_nm,
         y = "")
  
  return(plt)
}

cv_metrics_plt(recalls_all, "logistic regression")
cv_metrics_plt(precisions_all, "logistics regression")

#### Cross validation - Random forest ------------------------------------------

set.seed(125)
n_obs <- 1e5
subset <- sample_n(data_train[,c("is_fraud", "amt", "age", "category_fraud", "hour_fraud")], n_obs) # subset for the analysis
test_ratio <- 0.2
N <- 200

# Creating train/test sets
cv_mc <- crossv_mc(subset, N, test=test_ratio)

# fitting models on the training sets
models <- map(cv_mc$train, ~ randomForest(as_factor(is_fraud) ~ amt + age + category_fraud + hour_fraud,
                                          data=., ntree=100, classwt = c('0' = 0.25, '1' = 0.75)))

recalls_train <- map2_dbl(models, cv_mc$train,
                          ~confusion_matrix_rf(model=.x, data=as_tibble(.y), col='is_fraud')[2])

precisions_train <- map2_dbl(models, cv_mc$train,
                             ~confusion_matrix_rf(model=.x, data=as_tibble(.y), col='is_fraud')[1])

recalls_test <- map2_dbl(models, cv_mc$test,
                         ~confusion_matrix_rf(model=.x, data=as_tibble(.y), col='is_fraud')[2])

precisions_test <- map2_dbl(models, cv_mc$test,
                            ~confusion_matrix_rf(model=.x, data=as_tibble(.y), col='is_fraud')[1])

recalls_all <- rbind(tibble(model = 'train_data', recalls = recalls_train),
                     tibble(model = 'test_data', recalls = recalls_test))

precisions_all <- rbind(tibble(model = 'train_data', precisions = precisions_train),
                        tibble(model = 'test_data', precisions = precisions_test))

# Density plot of metrics
cv_metrics_plt(recalls_all, "random forest")
cv_metrics_plt(precisions_all, "random forest")














