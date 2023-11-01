library(correlation)
library(e1071)
library(randomForest)
library(adabag)
library(xgboost)
library(caret)
library(pROC)
library(pander)
library(ggplot2)

set.seed(1)

df <- read.csv('./completed_df.csv')

top_10_variables <- c("hospital_expire_flag", 
                      "aniongap_min", "aniongap_max",
                      "spo2_min", "ptt_max",
                      "bilirubin_total_max", "bilirubin_total_min", 
                      "sbp_min", "mbp_min",
                      "temperature_min", "bicarbonate_min")
data <- cbind(df[1],selected_columns)


# PCA --------------------------------------------------------------------

pca_result <- prcomp(data[, -1], center = TRUE, scale. = TRUE)
# 80%
explained_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_explained_variance <- cumsum(explained_variance_ratio)
num_components <- which(cum_explained_variance >= 0.8)[1]
print(paste("Number of principal components:", num_components))

data_pca <- cbind(data[1], pca_result$x[, 1:num_components])
data <- data_pca


# model -------------------------------------------------------------------

index <- createDataPartition(data$hospital_expire_flag, p = 0.7, list = FALSE)
train_set <- data[index, ]
test_set <- data[-index, ]

# classfication, not regression
train_set$hospital_expire_flag <- as.factor(train_set$hospital_expire_flag)
test_set$hospital_expire_flag <- as.factor(test_set$hospital_expire_flag)  
# SVM
svm_model <- svm(hospital_expire_flag ~ ., data = train_set)

# random forest
rf_model <- randomForest(hospital_expire_flag ~ ., data = train_set)

# AdaBoost
ada_model <- boosting(hospital_expire_flag ~ ., data = train_set, mfinal = 500, classvec = TRUE)

# XGBoost
xgb_train <- xgb.DMatrix(data = as.matrix(train_set[, -1]), label=(as.numeric(train_set$hospital_expire_flag)-1))
xgb_test <- xgb.DMatrix(data = as.matrix(test_set[, -1]), label=(as.numeric(test_set$hospital_expire_flag)-1))
# labels need to be numeric
xgb_model <- xgboost(data = xgb_train, nrounds = 100, objective = "binary:logistic")

# evaluation
svm_pred <- predict(svm_model, newdata = test_set)
rf_pred <- predict(rf_model, newdata = test_set)
ada_pred <- predict(ada_model, newdata = test_set) $class
ada_pred <- as.factor(ada_pred)


xgb_pred_prob <- predict(xgb_model, newdata = as.matrix(test_set[, -1]))

# Convert to discrete variable
threshold <- 0.5
xgb_pred <- ifelse(xgb_pred_prob >= threshold, 1, 0)
xgb_pred <- as.factor(xgb_pred)

# conf comparison

conf_matrix_svm <- confusionMatrix(svm_pred, test_set$hospital_expire_flag)
conf_matrix_rf <- confusionMatrix(rf_pred, test_set$hospital_expire_flag)
conf_matrix_ada <- confusionMatrix(ada_pred, test_set$hospital_expire_flag)
conf_matrix_xgb <- confusionMatrix(xgb_pred, test_set$hospital_expire_flag)

models <- c("SVM", "Random Forest", "AdaBoost", "XGBoost")
accuracies <- c(conf_matrix_svm$overall["Accuracy"],
                conf_matrix_rf$overall["Accuracy"],
                conf_matrix_ada$overall["Accuracy"],
                conf_matrix_xgb$overall["Accuracy"])
recalls <- c(recall(svm_pred, test_set$hospital_expire_flag),
             recall(rf_pred, test_set$hospital_expire_flag),
             recall(ada_pred, test_set$hospital_expire_flag),
             recall(xgb_pred, test_set$hospital_expire_flag))

# table
result_table <- data.frame(Model = models, Accuracy = accuracies, Recall = recalls)
pander::pander(result_table, style = "rmarkdown")