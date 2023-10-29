df <- read.csv('./completed_df.csv')
# 选前十个相关性最强的变量————————————
library(correlation)
cor_matrix <- correlation(df, method = "auto") # 计算Point-Biserial Correlation
cor_with_hospital_expire_flag <- cor_matrix[cor_matrix$Parameter1 == "hospital_expire_flag", ] # 获取与出院状态相关性的绝对值（不区分正负）
sorted_cor <- cor_with_hospital_expire_flag[order(abs(cor_with_hospital_expire_flag$r), decreasing = TRUE), ]  #排序
top_10_variables <- head(sorted_cor$Parameter2, 10) # 获取与出院状态相关性最高的前 10 个变量的名称
print(top_10_variables)   # 输出前 10 个变量名
selected_columns <- df[top_10_variables]
data <- cbind(df[1],selected_columns)

# 对选择的十个变量执行PCA-------------
pca_result <- prcomp(data[, -1], center = TRUE, scale. = TRUE)
# 选择能解释原始数据80%方差的主成分
explained_variance_ratio <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_explained_variance <- cumsum(explained_variance_ratio)
num_components <- which(cum_explained_variance >= 0.8)[1]
print(paste("Number of principal components:", num_components))

# 将原始数据替换为主成分得分
data_pca <- cbind(data[1], pca_result$x[, 1:num_components])
data <- data_pca

# 模型——————————————————————————————
library(e1071)     # 支持向量机
library(randomForest)  # 随机森林
library(adabag)     # AdaBoost
library(xgboost)    # XGBoost
library(caret)     # 用于模型比较
library(pander)  # 用于创建规范的表格
library(ggplot2)  # 用于绘制图表
set.seed(1)
# 假设你的数据框为 train_data（包含特征和目标变量）
# 数据准备和特征工程（根据你的数据和需求）
# 训练集和测试集
# 用于随机数种子
index <- createDataPartition(data$hospital_expire_flag, p = 0.7, list = FALSE)
train_set <- data[index, ]
test_set <- data[-index, ]
train_set$hospital_expire_flag <- as.factor(train_set$hospital_expire_flag)  #使其被视为分类问题，而不是回归问题
test_set$hospital_expire_flag <- as.factor(test_set$hospital_expire_flag)  
fitControl <- trainControl(method = "repeatedcv",number = 5,repeats = 5)   ## repeated 5 times
# SVM————————————————
# 训练 SVM 模型
default_svm_model <- svm(hospital_expire_flag ~ ., data = train_set) #默认参数下的svm模型
svm_model <- train(hospital_expire_flag ~ ., data = train_set, method = "svmRadial",trControl = fitControl,verbose = FALSE)
svm_best_params <- svm_model$bestTune  # 找到最佳参数组合
best_svm_model <- svm(hospital_expire_flag ~ ., data = train_set, C = svm_best_params$C, sigma = svm_best_params$sigma )

# svm模型评估
default_svm_pred <- predict(default_svm_model, newdata = test_set)
best_svm_pred <- predict(best_svm_model, newdata = test_set)
conf_matrix_default_SVM <- confusionMatrix(default_svm_pred, test_set$hospital_expire_flag)
conf_matrix_best_SVM <- confusionMatrix(best_svm_pred, test_set$hospital_expire_flag)
svm_models <- c("default_SVM", "best_SVM")
svm_accuracies <- c(conf_matrix_default_SVM$overall["Accuracy"],
                    conf_matrix_best_SVM$overall["Accuracy"])
svm_recalls <- c(recall(default_svm_pred, test_set$hospital_expire_flag),
                 recall(best_svm_pred, test_set$hospital_expire_flag))
svm_result_table <- data.frame(Model = svm_models, Accuracy = svm_accuracies, Recall = svm_recalls)  #甚至完全一样
svm_result_table


# 随机森林rf————————————————
# 训练 rf 模型
default_rf_model <- randomForest(hospital_expire_flag ~ ., data = train_set) #默认模型
rf_model <- train(hospital_expire_flag ~ ., data = train_set, method = "rf",trControl = fitControl,verbose = FALSE)
rf_best_params <- rf_model$bestTune  # 找到最佳参数组合
best_rf_model <- randomForest(hospital_expire_flag ~ ., data = train_set, mtry = rf_best_params$mtry)

# rf模型评估
default_rf_pred <- predict(default_rf_model, newdata = test_set)
best_rf_pred <- predict(best_rf_model, newdata = test_set)
conf_matrix_default_rf <- confusionMatrix(default_rf_pred, test_set$hospital_expire_flag)
conf_matrix_best_rf <- confusionMatrix(best_rf_pred, test_set$hospital_expire_flag)
rf_models <- c("default_rf", "best_rf")
rf_accuracies <- c(conf_matrix_default_rf$overall["Accuracy"],
                    conf_matrix_best_rf$overall["Accuracy"])
rf_recalls <- c(recall(default_rf_pred, test_set$hospital_expire_flag),
                 recall(best_rf_pred, test_set$hospital_expire_flag))
rf_result_table <- data.frame(Model = rf_models, Accuracy = rf_accuracies, Recall = rf_recalls) #甚至不如默认

rf_result_table
# AdaBoost————————————————
# 训练 ada 模型
default_ada_model <- boosting(hospital_expire_flag ~ ., data = train_set, mfinal = 500, classvec = TRUE) #默认模型
ada_model <- train(hospital_expire_flag ~ ., data = train_set, method = "ada",trControl = fitControl,verbose = FALSE)
ada_best_params <- ada_model$bestTune  # 找到最佳参数组合
best_ada_model <- boosting(hospital_expire_flag ~ ., data = train_set, iter = ada_best_params$iter, maxdepth = ada_best_params$maxdepth, nu = ada_best_params$nu )

# ada模型评估
default_ada_pred <- predict(default_ada_model, newdata = test_set)$class
best_ada_pred <- predict(best_ada_model, newdata = test_set)$class
default_ada_pred <- as.factor(default_ada_pred)
best_ada_pred <- as.factor(best_ada_pred)
conf_matrix_default_ada <- confusionMatrix(default_ada_pred, test_set$hospital_expire_flag)
conf_matrix_best_ada <- confusionMatrix(best_ada_pred, test_set$hospital_expire_flag)
ada_models <- c("default_ada", "best_ada")
ada_accuracies <- c(conf_matrix_default_ada$overall["Accuracy"],
                   conf_matrix_best_ada$overall["Accuracy"])
ada_recalls <- c(recall(default_ada_pred, test_set$hospital_expire_flag),
                recall(best_ada_pred, test_set$hospital_expire_flag))
ada_result_table <- data.frame(Model = ada_models, Accuracy = ada_accuracies, Recall = ada_recalls) #甚至不如默认
ada_result_table

# XGBoost--------------
xgb_train <- xgb.DMatrix(data = as.matrix(train_set[, -1]), label=(as.numeric(train_set$hospital_expire_flag)-1))
xgb_test <- xgb.DMatrix(data = as.matrix(test_set[, -1]), label=(as.numeric(test_set$hospital_expire_flag)-1))
default_xgb_model <- xgboost(data = xgb_train, nrounds = 100, objective = "binary:logistic")  #标签要数值格式。。

xgb_model <- train(hospital_expire_flag ~ ., data = train_set, method = "xgbTree",trControl = fitControl,verbose = FALSE) #这个又不用。。
xgb_best_params <- xgb_model$bestTune  # 找到最佳参数组合
best_xgb_model <- xgboost(data = xgb_train, nrounds = xgb_best_params$nrounds, maxdepth = xgb_best_params$maxdepth, eta = xgb_best_params$eta, gamma = xgb_best_params$gamma, colsample_bytree = xgb_best_params$colsample_bytree,
                          min_child_weight = xgb_best_params$min_child_weight, subsample = xgb_best_params$subsample, objective = "binary:logistic")

# xgb模型评估

default_xgb_pred <- predict(xgb_model, newdata = as.matrix(test_set[, -1]))
best_xgb_pred_prob <- predict(best_xgb_model, newdata = as.matrix(test_set[, -1]))
threshold <- 0.5
best_xgb_pred <- ifelse(best_xgb_pred_prob >= threshold, 1, 0)
best_xgb_pred <- as.factor(best_xgb_pred)

conf_matrix_default_xgb <- confusionMatrix(default_xgb_pred, test_set$hospital_expire_flag)
conf_matrix_best_xgb <- confusionMatrix(best_xgb_pred, test_set$hospital_expire_flag)
xgb_models <- c("default_rf", "best_rf")
xgb_accuracies <- c(conf_matrix_default_xgb$overall["Accuracy"],
                    conf_matrix_best_xgb$overall["Accuracy"])
xgb_recalls <- c(recall(default_xgb_pred, test_set$hospital_expire_flag),
                 recall(best_xgb_pred, test_set$hospital_expire_flag))
xgb_result_table <- data.frame(Model = xgb_models, Accuracy = xgb_accuracies, Recall = xgb_recalls) #甚至不如默认
xgb_result_table
