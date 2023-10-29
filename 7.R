#读取数据————————————————
library(ggplot2)
df <- read.csv('./mydata.csv')
#数据预处理——————————————
df_1 <- df[c(-9: -20)]#删除9-20
library(dplyr)
threshold <- 0.4
missing_p <- colMeans(is.na(df))
df <- df %>% select(which(missing_p <= threshold))  #删除缺失值超过40%的列
df_2 <- df[c(-1:-2,-4:-8)] #删除离散变量
library(mice)
imputed_df <- mice(df_2, method = "pmm", m = 5)  #预测均值匹配填补缺失值
completed_df <- complete(imputed_df, 1)  #直接用多重填补的生成的第一个数据集，实际上并没有多重填补

df_new <- cbind(df[c(1:20)],completed_df[c(2:length(completed_df))])



