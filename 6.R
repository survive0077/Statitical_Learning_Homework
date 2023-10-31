library(ggplot2)

df <- read.csv('./mydata.csv')
obs_num <- dim(df)[1]

selected <- c("hospital_expire_flag", 
              "aniongap_min", "aniongap_max",
              "spo2_min", "ptt_max",
              "bilirubin_total_max", "bilirubin_total_min", 
               "sbp_min", "mbp_min",
              "temperature_min", "bicarbonate_min")
ratio <- 0.8
split_point <- ceiling(ratio * obs_num)
data_set <- df[selected]
train_set <- data_set[1:split_point,]
test_set <- data_set[(split_point + 1):obs_num,]
rownames(test_set) <- c(1:dim(test_set)[1])


# use all 10 variables to fit ---------------------------------------------
# fit without intercept
fit_all <- glm(hospital_expire_flag ~ -1+
                 aniongap_min+aniongap_max+
                 bilirubin_total_max+bilirubin_total_min+
                 temperature_min+bicarbonate_min+
                 spo2_min+ptt_max+sbp_min+mbp_min, family = binomial(link = "logit"), data = train_set)
summary(fit_all)
coef(fit_all)
predprob_all <- predict.glm(fit_all, test_set, type="response")
result_all <- na.omit(predprob_all)


# use single variable to fit ----------------------------------------------
fit_single <- function(nm)
{
  data_temp <- data.frame(train_set$hospital_expire_flag, train_set[nm])
  colnames(data_temp) <- c("ifdead", "do")
  fit_temp <- glm(ifdead ~ -1+do, family = binomial(link = "logit"), data = data_temp)
  
  return(fit_temp)
}

pred_single <- function(nm, model)
{
  do_test <- data.frame(test_set$hospital_expire_flag, test_set[nm])
  colnames(do_test) <- c("ifdead", "do")
  pred_now <- predict.glm(model, do_test, type="response")
  
  return(pred_now)
}

do_num <- 11
selected[do_num]
fit_0 <- fit_single(selected[do_num])
summary(fit_0)
coef(fit_0)
pred_0 <-pred_single(selected[do_num], fit_0)

