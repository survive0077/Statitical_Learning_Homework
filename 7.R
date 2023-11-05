library(glmnet)
library(caret)
library(e1071)
library(ROCR)
library(ggpubr)
library(dplyr)


# missing data imputation -------------------------------------------------

df <- read.csv('./mydata.csv')
# delete 1-20 variables
df_1 <- df[c(-1: -20)]

# delete variables which miss more than 40% 
threshold <- 0.4
missing_p <- colMeans(is.na(df_1))
df_1 <- df_1 %>% select(which(missing_p <= threshold))

# predictive mean matching
imputed_df <- mice(df_1, method = "pmm", m = 5)
completed_df <- complete(imputed_df, 1)

# choose "alp_min" as an example
# blue : observed
# red : imputed
# view the imputed data set
strplot <- stripplot(imputed_df,alp_min~.imp,pch=20,cex=2)
# density map of the interpolated data set
denplot <- densityplot(imputed_df,~alp_min)
ggarrange(strplot,denplot,ncol = 2)

# save imputed data frame
df_new <- cbind(df[c("hospital_expire_flag")],completed_df)
# write.csv(df_new, file="1.csv", row.names = FALSE)

df <- read.csv('./completed_df.csv')

hospital_expire_flag <- df$hospital_expire_flag
top_10_variables <- c("aniongap_min", "aniongap_max",
                      "spo2_min", "ptt_max",
                      "bilirubin_total_max", "bilirubin_total_min", 
                      "sbp_min", "mbp_min",
                      "temperature_min", "bicarbonate_min")
features <- df[top_10_variables]



# find the smallest lambda ------------------------------------------------
cv_std <- cv.glmnet(as.matrix(features), hospital_expire_flag, type.measure="class", nfolds=10, family="binomial")
lambda <- cv_std$lambda.min
plot(cv_std)

# Ridge -------------------------------------------------------------------
logit_ridge_model <- glmnet(x = as.matrix(features), y = hospital_expire_flag, family = "binomial", alpha = 0)
ridge_coefficients <- coef(logit_ridge_model , s = lambda)

# Lasso -------------------------------------------------------------------
logit_lasso_model <- glmnet(x = as.matrix(features), y = hospital_expire_flag, family = "binomial", alpha = 1)
lasso_coefficients <- coef(logit_lasso_model, s = lambda)

attach(mtcars)
opar <- par(no.readonly = TRUE)
par(mfrow=c(1, 2))
plot(logit_ridge_model)
plot(logit_lasso_model)
par(opar)
detach(mtcars)

# PCA ---------------------------------------------------------------------
# use normalization
pca_model <- prcomp(features, scale = TRUE)

# Calculate cumulative variance contribution rate
explained_variance_ratio <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cum_explained_variance_ratio <- cumsum(explained_variance_ratio)

# Find the number of principal components that contribute 80% of the cumulative variance
k <- which(cum_explained_variance_ratio >= 0.8)[1]
pca_data_reduced <- as.data.frame(pca_model$x[,c(1:k)])

logit_pca_model <- glm(hospital_expire_flag ~ ., data = pca_data_reduced, family = "binomial")
summary(logit_pca_model)


# ROC ---------------------------------------------------------------------

logit_lasso_result <- predict(logit_lasso_model, newx = as.matrix(features), s = lambda)
pred_logit_lasso <- prediction(predictions = logit_lasso_result,labels = hospital_expire_flag, label.ordering = c(0,1))
pref_logit_lasso <-  performance(pred_logit_lasso,"tpr","fpr")

logit_ridge_result <- predict(logit_ridge_model, newx = as.matrix(features), s = lambda)
pred_logit_ridge <- prediction(predictions = logit_ridge_result,labels = hospital_expire_flag, label.ordering = c(0,1))
pref_logit_ridge <-  performance(pred_logit_ridge,"tpr","fpr")

logit_pca_result <- predict(logit_pca_model, newx = pca_data_reduced)
pred_logit_pca <- prediction(predictions = logit_pca_result,labels = hospital_expire_flag, label.ordering = c(0,1))
pref_logit_pca <-  performance(pred_logit_pca,"tpr","fpr")

plot(pref_logit_lasso,lwd=4)
lines(pref_logit_ridge@x.values[[1]],pref_logit_ridge@y.values[[1]],col="red",lwd=1)
lines(pref_logit_pca@x.values[[1]],pref_logit_pca@y.values[[1]],col="blue",lwd=2)
legend("bottomright",legend = c("logit_lasso", "logit_ridge", "logit_pca"), lwd=1, col = c("black", "red", "blue"))

