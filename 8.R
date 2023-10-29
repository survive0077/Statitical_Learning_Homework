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
# 训练 SVM 模型
svm_model <- svm(hospital_expire_flag ~ ., data = train_set)

# 训练随机森林模型
rf_model <- randomForest(hospital_expire_flag ~ ., data = train_set)

# 训练 AdaBoost 模型
ada_model <- boosting(hospital_expire_flag ~ ., data = train_set, mfinal = 500, classvec = TRUE)

# 训练 XGBoost 模型
xgb_train <- xgb.DMatrix(data = as.matrix(train_set[, -1]), label=(as.numeric(train_set$hospital_expire_flag)-1))
xgb_test <- xgb.DMatrix(data = as.matrix(test_set[, -1]), label=(as.numeric(test_set$hospital_expire_flag)-1))
xgb_model <- xgboost(data = xgb_train, nrounds = 100, objective = "binary:logistic")  #标签要数值格式。。

# 模型评估
svm_pred <- predict(svm_model, newdata = test_set)
rf_pred <- predict(rf_model, newdata = test_set)
ada_pred <- predict(ada_model, newdata = test_set) $class
ada_pred <- as.factor(ada_pred)


xgb_pred_prob <- predict(xgb_model, newdata = as.matrix(test_set[, -1]))
# 将类概率转换为离散分类
threshold <- 0.5
xgb_pred <- ifelse(xgb_pred_prob >= threshold, 1, 0)
xgb_pred <- as.factor(xgb_pred)

# 比较模型——————————————————————————

# 加载必要的包
library(pROC)  # 用于 ROC 曲线
library(pander)  # 用于创建规范的表格
library(ggplot2)  # 用于绘制图表

conf_matrix_svm <- confusionMatrix(svm_pred, test_set$hospital_expire_flag)
conf_matrix_rf <- confusionMatrix(rf_pred, test_set$hospital_expire_flag)
conf_matrix_ada <- confusionMatrix(ada_pred, test_set$hospital_expire_flag)
conf_matrix_xgb <- confusionMatrix(xgb_pred, test_set$hospital_expire_flag)

# 比较模型性能
models <- c("SVM", "Random Forest", "AdaBoost", "XGBoost")
accuracies <- c(conf_matrix_svm$overall["Accuracy"],
                conf_matrix_rf$overall["Accuracy"],
                conf_matrix_ada$overall["Accuracy"],
                conf_matrix_xgb$overall["Accuracy"])
recalls <- c(recall(svm_pred, test_set$hospital_expire_flag),
             recall(rf_pred, test_set$hospital_expire_flag),
             recall(ada_pred, test_set$hospital_expire_flag),
             recall(xgb_pred, test_set$hospital_expire_flag))

# 创建规范的表格
result_table <- data.frame(Model = models, Accuracy = accuracies, Recall = recalls)
pander::pander(result_table, style = "rmarkdown")