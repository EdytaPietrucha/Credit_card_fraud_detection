# dev.off()
main_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(main_path)

source("libraries.R")
source("functions.R")

#### Source data  --------------------------------------------------------------
# data_train <- fread(paste0(main_path,"/Data/fraudTrain.csv"))
data_train <- fread("C:/Users/edyta/Desktop/GitHub/repos/credit_card_fraud_detection//Data/fraudTrain.csv")
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

#### Exploratory Data Analysis  ------------------------------------------------
# Compute age of the customer ina day of transaction
data_train[, age := round(as.numeric(difftime(trans_date_trans_time, dob, units = "days")/365), 0)]

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
data_train_bin <- copy(data_train)
# data_train_bin <- data_train_bin[, ..temp] %>% 
#   apply(2, function(x) min_max_norm(x)) %>%
#   as.data.table() %>% 
#   cbind(data_train_bin %>% 
#           .[, -..temp])
data_train_bin[, (temp) := lapply(.SD, min_max_norm), .SDcols = temp] # reference!

# Correlation
cor(data_train[, ..temp])
ggcorr(data_train[, ..temp], label = T)
corrplot::corrplot(cor(data_train[, ..temp]), method = 'number')

####  Logistic regression  -----------------------------------------------------
set.seed(125)
data_train_bin_cv <- resample_partition(data_train_bin, p = c(valid = 0.3, train = 0.7))
train_bin_cv <- data_train_bin[data_train_bin_cv$train$idx,]
valid_bin_cv <- data_train_bin[data_train_bin_cv$valid$idx,]

logreg <- glm(is_fraud ~ 
                category_fraud + amt + hour_fraud + age:hour_fraud + amt:hour_fraud + amt:category_fraud + age:category_fraud,
              data = train_bin_cv, family = binomial)
summary(logreg)

cutoff_lvl <- find_cutoff(logreg, train_bin_cv, 'is_fraud', 0.001, 0.1, 0.001)

conf_mat_logreg_train <- confusion_matrix(logreg, train_bin_cv, col = "is_fraud", cutoff = 0.003)
conf_mat_logreg_valid <- confusion_matrix(logreg, valid_bin_cv, col = "is_fraud", cutoff = 0.003)

#### Logistic regression with PCA ----------------------------------------------
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

#### Random forest -------------------------------------------------------------
# CV on unnormalized data
train_cv <- data_train[data_train_bin_cv$train$idx,]
valid_cv <- data_train[data_train_bin_cv$valid$idx,]

set.seed(125)
subset_tr <- sample_n(train_cv, 10^5)
rf_params <- find_classwt(subset_tr)

forest <- randomForest(as_factor(is_fraud) ~ amt + age + category_fraud + hour_fraud,
                       data=train_cv, 
                       ntree=100, 
                       classwt = c('0' = 0.25, '1' = 0.75))

conf_mat_forest_train <- confusion_matrix(forest, train_cv, 'is_fraud', model_type = "forest")
conf_mat_forest_valid <- confusion_matrix(forest, valid_cv, 'is_fraud', model_type = "forest")

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
                          ~confusion_matrix(model=.x, data=as_tibble(.y), col='is_fraud', model_type = "forest")[2])

precisions_train <- map2_dbl(models, cv_mc$train,
                             ~confusion_matrix(model=.x, data=as_tibble(.y), col='is_fraud', model_type = "forest")[1])

recalls_test <- map2_dbl(models, cv_mc$test,
                         ~confusion_matrix(model=.x, data=as_tibble(.y), col='is_fraud', model_type = "forest")[2])

precisions_test <- map2_dbl(models, cv_mc$test,
                            ~confusion_matrix(model=.x, data=as_tibble(.y), col='is_fraud', model_type = "forest")[1])

recalls_all <- rbind(tibble(model = 'train_data', recalls = recalls_train),
                     tibble(model = 'test_data', recalls = recalls_test))

precisions_all <- rbind(tibble(model = 'train_data', precisions = precisions_train),
                        tibble(model = 'test_data', precisions = precisions_test))

# Density plot of metrics
cv_metrics_plt(recalls_all, "random forest")
cv_metrics_plt(precisions_all, "random forest")

#### Performance check on testing dataset --------------------------------------
data_test <- fread("C:/Users/edyta/Desktop/GitHub/repos/credit_card_fraud_detection//Data/fraudTest.csv")

# Logistic regression
data_test_logreg <- data_preprocess_logreg(data_test)
logreg <- glm(is_fraud ~ 
                category_fraud + amt + hour_fraud + age:hour_fraud + amt:hour_fraud + amt:category_fraud + age:category_fraud,
              data = data_test_logreg, family = binomial)
summary(logreg)

conf_mat_logreg_test <- confusion_matrix(logreg, data_test_logreg, col = "is_fraud", cutoff = 0.003)
conf_mat_logreg_test

# Random forest
data_test_forest <- data_preprocess_forest(data_test)
forest <- randomForest(as_factor(is_fraud) ~ amt + age + category_fraud + hour_fraud,
              data = data_test_forest, ntree=100, classwt = c('0' = 0.25, '1' = 0.75))
summary(forest)

conf_mat_forest_test <- confusion_matrix(forest, data_test_forest, col = "is_fraud", cutoff = 0.0025, model_type = "forest")
conf_mat_forest_test

#### Conclusion  ---------------------------------------------------------------
model <- c('logistic regression', 'random forest')
quality <- c('not overfited', 'significantly overfited')
recall_train <- c(round(conf_mat_logreg_train[2], 3),round(conf_mat_forest_train[2],3))
precision_train <- c(round(conf_mat_logreg_train[1], 3),round(conf_mat_forest_train[1],3))
recall_test <- c(round(conf_mat_logreg_test[2], 3),round( conf_mat_forest_test[2],3))
precision_test <- c(round(conf_mat_logreg_test[1], 3),round( conf_mat_forest_test[1],3))
tbl <- tibble(Model = model,
              Quality = quality,
              Recall_train = recall_train,
              Precision_train = precision_train,
              Recall_test = recall_test,
              Precision_test = precision_test)
