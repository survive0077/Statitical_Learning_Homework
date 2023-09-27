library(ggplot2)


df <- read.csv('./mydata.csv')
death_list <- df$hospital_expire_flag

prior_num <- 100
prior_list <- death_list[1:prior_num]
prior_mean <- mean(prior_list)
prior_var <- var(prior_list)
prior_a <- abs(((1 - prior_mean) / prior_var - 1 / prior_mean) * prior_mean ^ 2)
prior_b <- abs(prior_a * (1 / prior_mean - 1))

theta_list <- c()
theta_std <- c()
for (i in death_list[(prior_num + 1):(length(death_list))])
{
  prior_a <- prior_a + i
  prior_b <- prior_b + 1 - i
  prior_list <- c(prior_list, i)
  theta <- prior_a / (prior_a + prior_b)
  # std <- sqrt(prior_a * prior_b / ((prior_a + prior_b) ^ 2 * (prior_a + prior_b + 1)))
  std <- sd(prior_list)
  theta_list <- c(theta_list, theta)
  theta_std <- c(theta_std, std)
}

theta_mean <- mean(theta_list)
theta_sd <- sd(theta_list)
aplha <- 0.05
# confidence <- qnorm(0.5 + aplha / 2) * theta_sd / sqrt(length(theta_list))

confidence_list <- c()
for (i in c((prior_num + 1):(length(death_list))))
{
  # unknown sigma, calculate mu
  # t-distribution
  confidence <- qt(1 - aplha / 2, i - 1) * theta_std[i - prior_num] / sqrt(i)
  confidence_list <- c(confidence_list, confidence)
}


# sampling ----------------------------------------------------------------
begin <- 1
step <- 50
sample_index <- seq(from=begin, to=length(theta_list), by=step)
theta_list <- theta_list[sample_index]
confidence_list <- confidence_list[sample_index]


df1 <- data.frame(observations=c(1:length(theta_list)), theta=theta_list)
df2 <- data.frame(observations=c(1:length(theta_list)), theta_up=theta_list+confidence_list, theta_down=theta_list-confidence_list)
ggplot(data = df1, mapping = aes(x = observations, y = theta))+
  geom_line()+labs(x="observations",y="theta")+
  geom_hline(aes(yintercept=theta_mean), colour="#BB0000", linetype="dashed")+
  # coord_cartesian(xlim =c(0, ((length(death_list) - begin) / step)), ylim = c(0.308, 1))+
  scale_x_continuous(breaks=c(0,25,50,75,100,125),labels=c(0,25,50,75,100,125)*50,)+
  geom_text(aes(x=100,y=0.4),label="theta=0.323254",cex=6)+
  theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'),legend.position = c(0.15,0.85))+
  geom_ribbon(data = df2, mapping = aes(ymin=theta_down,ymax=theta_up),fill="grey70",alpha=0.3)


# PDF ---------------------------------------------------------------------

set.seed(13579)                                    
N <- 10000                                         
y_rbeta <- rbeta(N, shape1 = 1975, shape2 = 4198)        

plot(density(y_rbeta), main = "beta Distribution in R")
