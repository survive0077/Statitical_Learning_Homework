library(ggplot2)
library(ggcorrplot)
library(mice)

df <- read.csv('./mydata.csv')
# missing pattern
md.pattern(df[c(0:100),c(80:100)],rotate.names =  TRUE)
ifdead <- df$hospital_expire_flag
variable_list <- df[-c(1:20)]

cal_cor <- function(dolist)
{
  do_df <- data.frame(flag=ifdead, vb=dolist)
  
  na_bool <- is.na(do_df$vb)
  do_df <- do_df[!na_bool,]
  
  value <- cor(do_df$flag, do_df$vb)
  
  return(round(value, 3))
}

cor_list <- c()
for (i in names(variable_list))
{
  # single variable data frame to vector
  dolist <- c(t(variable_list[i]))
  cor_v <- cal_cor(dolist)
  cor_list <- c(cor_list, cor_v)
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
