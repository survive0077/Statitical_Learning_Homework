df <- read.csv('./completed_df.csv')
#数据降维————————————————
hospital_expire_flag <- df$hospital_expire_flag # 分离目标变量
features <- df[, -which(names(df) == "hospital_expire_flag")] #其他变量
#————————————Ridge
library(glmnet)

#交叉验证取lambda
cv_std <- cv.glmnet(as.matrix(features), hospital_expire_flag, type.measure="class", nfolds=10, family="binomial")
lambda <- cv_std$lambda.min
logit_ridge_model <- glmnet(x = as.matrix(features), y = hospital_expire_flag, family="binomial", alpha=0)
ridge_coefficients <- coef(logit_ridge_model , s = lambda) # 提取系数

#————————————Lasso
logit_lasso_model <- glmnet(x = as.matrix(features), y = hospital_expire_flag, family="binomial", alpha = 1)
lasso_coefficients <- coef(logit_lasso_model, s = lambda) # 提取系数


#————————————PCA
library(caret)
library(e1071)
pca_model <- prcomp(features, scale = TRUE)
pca_data <- pca_model$x
logit_pca_model <- glm(hospital_expire_flag ~ ., data = as.data.frame(pca_data), family = "binomial")
summary(logit_pca_model)
