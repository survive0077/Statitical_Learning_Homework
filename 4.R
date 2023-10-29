library(ggplot2)
library(ggpubr)
library(ROCR)

df <- read.csv('./mydata.csv')
death_list <- df$hospital_expire_flag
len <- length(death_list)
set.seed(13579) 

# Q4.1 --------------------------------------------------------------------


mu <- c(5,0)
sd <- c(2,1)

Gaussian_random <- vector()
norm1_list <- vector()
norm2_list <- vector()

for (i in c(1:len))
{
  if (death_list[i])
  {
    temp <- rnorm(n=1,mean=mu[1],sd=sd[1])
    Gaussian_random[i] <- temp
    norm1_list <- c(norm1_list,temp)
  }
  else
  {
    temp <- rnorm(n=1,mean=mu[2],sd=sd[2])
    Gaussian_random[i] <- temp
    norm2_list <- c(norm2_list,temp)
  }
}

df1 <- data.frame(guassian=Gaussian_random,ifdead=as.character(death_list))

pic1 <- ggplot(data = df1,mapping = aes(x = guassian))+
          geom_density(alpha=0.5,size=1)+labs(x="x")+
          theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'),legend.position = c(0.15,0.85))

pic2 <- ggplot(data = df1,mapping = aes(x = guassian,fill = ifdead))+
          geom_density(alpha=0.5,size=1)+labs(x="x")+
          theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'),legend.position = c(0.8,0.85))


ggarrange(pic1,pic2,ncol = 2)



# Q4.2 --------------------------------------------------------------------
f0 <- dnorm(Gaussian_random,mean=mu[2],sd=sd[2])
f1 <- dnorm(Gaussian_random,mean=mu[1],sd=sd[1])
f <- f1 / (f0 + f1)

# type1_error <- 0.05
# z_score <- qnorm((1 - type1_error / 2),mean=mu[2],sd=sd[2])
# quantile <- (z_score - ((mu[1] - mu[2]) * sqrt(len - sum(death_list)) / sd[2]))
# type2_error <- pnorm(quantile)

error_1 <- 0
error_2 <- 0
for (i in c(1:len))
{
  if (death_list[i])
  {
    if (f[i] < 0.5)
      error_1 <- error_1 + 1
  }
  else
  {
    if (f[i] >= 0.5)
      error_2 <- error_2 + 1
  }
}

# type1_error = 0.05572139
type1_error <- error_1 / sum(death_list)
# type2_error = 0.02275393
type2_error <- error_2 / (len - sum(death_list))

pred <- ifelse(f >= 0.5, "P", "N")
df2 <- data.frame(ifdead=death_list, pred=pred, f=f)

tab <- xtabs(~ifdead+pred,data = df2)

#       pred
# ifdead    N    P
#      0 4166   97
#      1  112 1898

TP <- tab[2,2]
FP <- tab[1,2]
TN <- tab[1,1]
FN <- tab[2,1]
precision <- TP / (TP + FP)
recall <- TP / (TP +FN)
# F1 = 0.9478152
F1 <- 2 * precision * recall / (precision + recall)


pred1 <- prediction(predictions = df2$f,labels = df2$ifdead,label.ordering = c(0,1))
# AUC = 0.9896805
auc <- performance(pred1, "auc")@y.values[[1]]
pref <-  performance(pred1,"tpr","fpr")
plot(pref)


# Q4.3 --------------------------------------------------------------------

mu_eva <- c(4, 3)
sd_eva <- c(2, 1)
lambda_eva <- 0.1
gamma1_eva <- vector()
gamma2_eva <- vector()
err <- 1000000

while(err > 0.05)
{
  # -------------- E step --------------
  for (i in c(1:len))
  {
    temp_1 <- lambda_eva * dnorm(Gaussian_random[i], mean=mu_eva[1], sd=sd_eva[1])
    temp_2 <- (1 - lambda_eva) * dnorm(Gaussian_random[i], mean=mu_eva[2], sd=sd_eva[2])
    # calculate the probability of each point belong to each Gaussian
    # sum to 1
    gamma1_eva[i] <- temp_1 / (temp_1 + temp_2)
    gamma2_eva[i] <- 1 - gamma1_eva[i]
  }
  # -------------- M step --------------
  # the number of points each Gaussian has
  n_1 <- sum(gamma1_eva) + 0.001
  n_2 <- sum(gamma2_eva) + 0.001
  
  # backup old value
  lambda_before <- lambda_eva
  mu_before <- mu_eva
  sd_before <- sd_eva
  
  # update the ratio of each Gaussian in completed data
  lambda_eva <- round((n_1 / len), 2)
  
  # update the mean of each Gaussian
  mu_eva[1] <- sum(gamma1_eva * Gaussian_random) / n_1
  mu_eva[2] <- sum(gamma2_eva * Gaussian_random) / n_2
  
  # update the std of each Gaussian
  sd_eva[1] <- sqrt(sum(gamma1_eva * (Gaussian_random - mu_eva[1]) ^ 2) / n_1)
  sd_eva[2] <- sqrt(sum(gamma2_eva * (Gaussian_random - mu_eva[2]) ^ 2) / n_2)
  
  # calculate the error
  err <- sum(abs(mu_eva - mu_before)) + sum(abs(sd_eva - sd_before))
}
mu_eva
sd_eva
lambda_eva

x <- seq(-20,20,0.1)
re <- lambda_eva*dnorm(x,mu_eva[1],sd_eva[1]) + (1-lambda_eva)*dnorm(x,mu_eva[2],sd_eva[2])
plot(density(df1$guassian))
lines(x,re,col="red",lwd=2)
legend("topright",legend = c("origin", "EM"), lwd=1, col = c("black", "red"))


# lambda = 0.32  lambda_eva = 0.34
# mu = c(5,0)    mu_eva = c(4.8399788 -0.0249322)
# sd = c(2,1)    sd_eva = c(2.1099803 0.9673895)
f0_ <- dnorm(Gaussian_random,mean=mu_eva[2],sd=sd_eva[2])
f1_ <- dnorm(Gaussian_random,mean=mu_eva[1],sd=sd_eva[1])
f_ <- f1_ / (f0_ + f1_)
pred_ <- ifelse(f_ >= 0.5, "P", "N")
df3 <- data.frame(ifdead=death_list, pred=pred_, f=f_)

tab_ <- xtabs(~ifdead+pred_,data = df3)
#       pred_
# ifdead    N    P
#      0 4128  135
#      1  102 1908
pred2 <- prediction(predictions = f_,labels = df3$ifdead,label.ordering = c(0,1))
auc_ <- performance(pred2, "auc")@y.values[[1]]
pref_ <-  performance(pred2,"tpr","fpr")
plot(pref_,lwd=3)
lines(pref@x.values[[1]],pref@y.values[[1]],col="red")

