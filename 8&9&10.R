library(correlation)
library(e1071)
library(randomForest)
library(adabag)
library(xgboost)
library(caret)
library(pROC)
library(pander)
library(ggplot2)
library(mice)

set.seed(1)

df <- read.csv('./completed_df.csv')
df1 <- read.csv('./mydata.csv')

continus_base <- df1[c("weight_admit", "height", "admission_age")]
# fill in the NA's
imputed <- mice(continus_base, method = "pmm", m = 5)
completed_continus_base <- complete(imputed, 1)

top_10_variables <- c("aniongap_min", "aniongap_max",
                      "spo2_min", "ptt_max",
                      "bilirubin_total_max", "bilirubin_total_min", 
                      "sbp_min", "mbp_min",
                      "temperature_min", "bicarbonate_min")

# one-hot to base line variables
# if there are 8 classes, then will get 7 0-1 class and 1 base class

# UNKNOWN,OTHER,UNABLE TO OBTAIN are seen as one class and dropped
ethnicity_onehot <- (model.matrix(~ethnicity-1,df1) %>% as.data.frame())[-c(5,6,7)]
# F is dropped
gender_onehot <- (model.matrix(~gender-1,df1) %>% as.data.frame())[-c(1)]

selected_columns <- df[top_10_variables]
data <- cbind(df[1],selected_columns, ethnicity_onehot, gender_onehot, completed_continus_base)

calculate_recall <- function(predicted, actual) {
  true_positive <- sum(predicted == 1 & actual == 1)
  false_negative <- sum(predicted == 0 & actual == 1)
  recall <- true_positive / (true_positive + false_negative)
  
  return(recall)
}



# PCA --------------------------------------------------------------------

pca_result <- prcomp(data[, -1], center = TRUE, scale. = TRUE)
# 80%
explained_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_explained_variance <- cumsum(explained_variance_ratio)
num_components <- which(cum_explained_variance >= 0.8)[1]

data_pca <- cbind(data[1], pca_result$x[, 1:num_components])


# Q8 model -------------------------------------------------------------------

index <- createDataPartition(data_pca$hospital_expire_flag, p = 0.7, list = FALSE)
train_set <- data_pca[index, ]
test_set <- data_pca[-index, ]
threshold <- 0.5

# classfication, not regression
train_set$hospital_expire_flag <- as.factor(train_set$hospital_expire_flag)
test_set$hospital_expire_flag <- as.factor(test_set$hospital_expire_flag)  

# SVM
svm_model <- svm(hospital_expire_flag ~ ., data = train_set)

# random forest
rf_model <- randomForest(hospital_expire_flag ~ ., data = train_set)

# AdaBoost
ada_model <- boosting(hospital_expire_flag ~ ., data = train_set, mfinal = 100, classvec = TRUE)

# XGBoost
# as.numeric(as.factor(c(0,0,1))) ->  [1] 1 1 2
# -1 is to start counting from 0
xgb_train <- xgb.DMatrix(data = as.matrix(train_set[, -1]), label=(as.numeric(train_set$hospital_expire_flag)-1))
xgb_test <- xgb.DMatrix(data = as.matrix(test_set[, -1]), label=(as.numeric(test_set$hospital_expire_flag)-1))
# labels need to be numeric
xgb_model <- xgboost(data = xgb_train, nrounds = 100, objective = "binary:logistic")

# evaluation
svm_pred <- predict(svm_model, newdata = test_set)
rf_pred <- predict(rf_model, newdata = test_set)
ada_pred <- as.factor(predict(ada_model, newdata = test_set)$class)

# Convert to discrete variable
xgb_pred_prob <- predict(xgb_model, newdata = as.matrix(test_set[, -1]))
xgb_pred <- as.factor(ifelse(xgb_pred_prob >= threshold, 1, 0))

# confusion Matrix

conf_matrix_svm <- confusionMatrix(svm_pred, test_set$hospital_expire_flag)
conf_matrix_rf <- confusionMatrix(rf_pred, test_set$hospital_expire_flag)
conf_matrix_ada <- confusionMatrix(ada_pred, test_set$hospital_expire_flag)
conf_matrix_xgb <- confusionMatrix(xgb_pred, test_set$hospital_expire_flag)

models <- c("SVM", "Random Forest", "AdaBoost", "XGBoost")

# confusion Matrix use 0 as positive
# use custom function to calculate recall
accuracies <- c(conf_matrix_svm$overall["Accuracy"],
                conf_matrix_rf$overall["Accuracy"],
                conf_matrix_ada$overall["Accuracy"],
                conf_matrix_xgb$overall["Accuracy"])
recalls <- c(calculate_recall(svm_pred, test_set$hospital_expire_flag),
             calculate_recall(rf_pred, test_set$hospital_expire_flag),
             calculate_recall(ada_pred, test_set$hospital_expire_flag),
             calculate_recall(xgb_pred, test_set$hospital_expire_flag))

# table
result_table <- data.frame(Model = models, Accuracy = accuracies, Recall = recalls)
pander::pander(result_table, style = "rmarkdown")

# without base line variables
##########################################
#  |     Model     | Accuracy | Recall |
#  |:-------------:|:--------:|:------:|
#  |      SVM      |  0.715   | 0.2124 |
#  | Random Forest |  0.7108  | 0.3399 |
#  |   AdaBoost    |  0.7209  | 0.3186 |
#  |    XGBoost    |  0.6874  | 0.3186 |
##########################################

# with base line variables
##########################################
#  |     Model     | Accuracy | Recall |
#  |:-------------:|:--------:|:------:|
#  |      SVM      |  0.7267  | 0.2401 |
#  | Random Forest |  0.7299  | 0.351  |
#  |   AdaBoost    |  0.7193  | 0.3593 |
#  |    XGBoost    |  0.7033  | 0.3758 |
##########################################

# Q9 k-folds model----------------------------------------------------------------------

# repeated 5 times
fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 1)

# SVM
svm_warmup <- train(hospital_expire_flag ~ ., data = train_set, method = "svmRadial",trControl = fitControl,verbose = FALSE)
svm_best_params <- svm_warmup$bestTune
best_svm_model <- svm(hospital_expire_flag ~ ., data = train_set, C = svm_best_params$C, sigma = svm_best_params$sigma )

# random forest
rf_warmup <- train(hospital_expire_flag ~ ., data = train_set, method = "rf",trControl = fitControl,verbose = FALSE)
rf_best_params <- rf_warmup$bestTune
best_rf_model <- randomForest(hospital_expire_flag ~ ., data = train_set, mtry = rf_best_params$mtry)

# AdaBoost
ada_warmup <- train(hospital_expire_flag ~ ., data = train_set, method = "ada",trControl = fitControl,verbose = FALSE)
ada_best_params <- ada_warmup$bestTune
best_ada_model <- boosting(hospital_expire_flag ~ ., data = train_set, iter = ada_best_params$iter, maxdepth = ada_best_params$maxdepth, nu = ada_best_params$nu)

# XGBoost
xgb_warmup <- train(hospital_expire_flag ~ ., data = train_set, method = "xgbTree",trControl = fitControl,verbose = FALSE)
xgb_best_params <- xgb_warmup$bestTune
best_xgb_model <- xgboost(data = xgb_train, nrounds = xgb_best_params$nrounds, maxdepth = xgb_best_params$maxdepth, 
                          eta = xgb_best_params$eta, gamma = xgb_best_params$gamma, colsample_bytree = xgb_best_params$colsample_bytree,
                          min_child_weight = xgb_best_params$min_child_weight, subsample = xgb_best_params$subsample, objective = "binary:logistic")

# evaluation
best_svm_pred <- predict(best_svm_model, newdata = test_set)
best_rf_pred <- predict(best_rf_model, newdata = test_set)
best_ada_pred <- as.factor(predict(best_ada_model, newdata = test_set)$class)

best_xgb_pred_prob <- predict(best_xgb_model, newdata = as.matrix(test_set[, -1]))
best_xgb_pred <- as.factor(ifelse(best_xgb_pred_prob >= threshold, 1, 0))

# confusion Matrix
conf_matrix_best_SVM <- confusionMatrix(best_svm_pred, test_set$hospital_expire_flag)
conf_matrix_best_rf <- confusionMatrix(best_rf_pred, test_set$hospital_expire_flag)
conf_matrix_best_ada <- confusionMatrix(best_ada_pred, test_set$hospital_expire_flag)
conf_matrix_best_xgb <- confusionMatrix(best_xgb_pred, test_set$hospital_expire_flag)

# table

# without base line variables
# > svm_result_table
# Model  Accuracy    Recall
# 1 default_SVM 0.7150452 0.2124183
# 2    best_SVM 0.7150452 0.2124183

# with base line variables
# Model  Accuracy    Recall
# 1 default_SVM 0.7267411 0.2400662
# 2    best_SVM 0.7267411 0.2400662

svm_accuracies <- c(conf_matrix_svm$overall["Accuracy"], conf_matrix_best_SVM$overall["Accuracy"])
svm_recalls <- c(calculate_recall(svm_pred, test_set$hospital_expire_flag), calculate_recall(best_svm_pred, test_set$hospital_expire_flag))
svm_result_table <- data.frame(Model = c("default_SVM", "best_SVM"), Accuracy = svm_accuracies, Recall = svm_recalls)

# without base line variables
# > rf_result_table
# Model  Accuracy    Recall
# 1 default_rf 0.7107921 0.3398693
# 2    best_rf 0.7145136 0.3398693

# with base line variables
# Model  Accuracy    Recall
# 1 default_rf 0.7299309 0.3509934
# 2    best_rf 0.7320574 0.3625828

rf_accuracies <- c(conf_matrix_rf$overall["Accuracy"], conf_matrix_best_rf$overall["Accuracy"])
rf_recalls <- c(calculate_recall(rf_pred, test_set$hospital_expire_flag), calculate_recall(best_rf_pred, test_set$hospital_expire_flag))
rf_result_table <- data.frame(Model = c("default_rf", "best_rf"), Accuracy = rf_accuracies, Recall = rf_recalls)

# without base line variables
# > ada_result_table
# Model  Accuracy    Recall
# 1 default_ada 0.7208931 0.3186275
# 2    best_ada 0.7118554 0.3120915

# with base line variables
# Model  Accuracy    Recall
# 1 default_ada 0.7192982 0.3592715
# 2    best_ada 0.7145136 0.3642384

ada_accuracies <- c(conf_matrix_ada$overall["Accuracy"], conf_matrix_best_ada$overall["Accuracy"])
ada_recalls <- c(calculate_recall(ada_pred, test_set$hospital_expire_flag), calculate_recall(best_ada_pred, test_set$hospital_expire_flag))
ada_result_table <- data.frame(Model = c("default_ada", "best_ada"), Accuracy = ada_accuracies, Recall = ada_recalls)

# without base line variables
# > xgb_result_table
# Model  Accuracy    Recall
# 1 default_xgb 0.6874003 0.3186275
# 2    best_xgb 0.7139819 0.3218954

# with base line variables
# > xgb_result_table
# Model  Accuracy    Recall
# 1 default_xgb 0.7033493 0.3758278
# 2    best_xgb 0.6889952 0.3526490

xgb_accuracies <- c(conf_matrix_xgb$overall["Accuracy"], conf_matrix_best_xgb$overall["Accuracy"])
xgb_recalls <- c(calculate_recall(xgb_pred, test_set$hospital_expire_flag), calculate_recall(best_xgb_pred, test_set$hospital_expire_flag))
xgb_result_table <- data.frame(Model = c("default_xgb", "best_xgb"), Accuracy = xgb_accuracies, Recall = xgb_recalls)



