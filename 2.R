library(ggplot2)
library(ggpubr)

df <- read.csv('./mydata.csv')

# function ----------------------------------------------------------------

data_clean <- function(dolist, if_3sigma=FALSE) 
{
  vec_sum <- vector()
  vec_ddsum <- vector()
  do_df <- data.frame(target=dolist,ifdead=df$hospital_expire_flag)
  na_bool <- is.na(do_df$target)
  do_df <- do_df[!na_bool,]
  # 3 sigma clean
  if(if_3sigma)
  {
    mu <- mean(do_df$target)
    std <- sd(do_df$target)
    do_df <- do_df[(mu - 3 * std < do_df$target) & (do_df$target < mu + 3 * std),]
  }
  
  obs_num <- dim(do_df)[1]
  # repair row index
  rownames(do_df) <- c(1:obs_num)
  
  return(do_df)
}


# gender chi2 ----------------------------------------------------------------

df_gender <- data_clean(dolist = df$gender, if_3sigma = FALSE)
table_gender <- table(df_gender$ifdead, df_gender$target)
gender_chi <- chisq.test(table_gender)
gender_chi


# age anova ---------------------------------------------------------------

df_age <- data_clean(dolist = df$admission_age, if_3sigma = FALSE)
age_aov <- aov(target ~ ifdead, data = df_age)
summary(age_aov)


# height anova ------------------------------------------------------------

df_height <- data_clean(dolist = df$height, if_3sigma = TRUE)
height_aov <- aov(target ~ ifdead, data = df_height)
summary(height_aov)



# weight anova ------------------------------------------------------------

df_weight <- data_clean(dolist = df$weight_admit, if_3sigma = TRUE)
weight_aov <- aov(target ~ ifdead, data = df_weight)
summary(weight_aov)


# ethnicity chi2 ----------------------------------------------------------

df_ethnicity <- data_clean(dolist = df$ethnicity, if_3sigma = FALSE)
table_ethnicity <- table(df_ethnicity$ifdead, df_ethnicity$target)
ethnicity_chi <- chisq.test(table_ethnicity)
ethnicity_chi
