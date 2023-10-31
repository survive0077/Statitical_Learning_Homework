library(ggplot2)
library(ggcorrplot)
library(mice)
library(correlation)
library(dplyr)


df <- read.csv('./mydata.csv')
ifdead <- df$hospital_expire_flag

# missing pattern
md.pattern(df[c(0:100),c(80:100)],rotate.names =  TRUE)

# delete variables which miss more than 40% 
threshold <- 0.40
missing_p <- colMeans(is.na(df[-c(1:20)]))
variable_list <- df[-c(1:20)] %>% select(which(missing_p <= threshold))  


cal_cor <- function(dolist)
{
  do_df <- data.frame(flag=ifdead, vb=dolist)
  
  na_bool <- is.na(do_df$vb)
  do_df <- do_df[!na_bool,]
  
  corr <- correlation(do_df, method = "auto", include_factors = TRUE)
  value_method <- c(corr[[3]], corr[[10]])
  
  return(value_method)
}

cor_list <- c()
method_list <- c()
for (i in names(variable_list))
{
  # single variable data frame to vector
  dolist <- c(t(variable_list[i]))
  value_method <- cal_cor(dolist)
  cor_list <- c(cor_list, round(as.numeric(value_method[1]), 3))
  method_list <- c(method_list, value_method[2])
  
}
cor_frame <- data.frame(variable_name=names(variable_list), cor_value=cor_list)
# sort by abs
cor_frame <- cor_frame[order(abs(cor_frame$cor_value), decreasing = TRUE),]


# use cor(use = "pairwise.complete.obs") to drop NA

# a<-t(cor(ifdead,variable_list,use = "pairwise.complete.obs"))
# a <- data.frame(nm=names(variable_list),ab=a)
# a <- a[order(a$ab, decreasing = TRUE),]

selected <- cor_frame$variable_name[1:10]
# if NA, drop that line
cor_map <- data.frame(cor(cbind(ifdead, variable_list[selected]),use = "pairwise.complete.obs"))

ggcorrplot(corr=cor_map, method = "square", lab = TRUE)
