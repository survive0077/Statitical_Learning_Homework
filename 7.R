library(ggplot2)
library(dplyr)
library(mice)
library(ggpubr)

df <- read.csv('./mydata.csv')
stripplot(imp_data,bmi~.imp,pch=20,cex=2)
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

df_new <- cbind(df[c("hospital_expire_flag")],completed_df)



