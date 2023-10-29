library(ggplot2)


df <- read.csv('./mydata.csv')
death_list <- df$hospital_expire_flag

prior_num <- 100
prior_list <- death_list[1:prior_num]
prior_mean <- mean(prior_list)
prior_var <- var(prior_list)
# prior_a <- abs(((1 - prior_mean) / prior_var - 1 / prior_mean) * prior_mean ^ 2)
prior_a <- 7
# prior_b <- abs(prior_a * (1 / prior_mean - 1))
prior_b <- 13

alpha <- 0.05
theta_list <- c()
confidence_upper <- c()
confidence_lower <- c()

for (i in death_list[(prior_num + 1):(length(death_list))])
{
  prior_a <- prior_a + i
  prior_b <- prior_b + 1 - i
  prior_list <- c(prior_list, i)
  theta <- prior_a / (prior_a + prior_b)
  # std <- sqrt(prior_a * prior_b / ((prior_a + prior_b) ^ 2 * (prior_a + prior_b + 1)))
  theta_list <- c(theta_list, theta)
  
  # 100*(1-alpha)% Bayesian credible interval for theta
  # the interpretation of a Bayesian credible interval is different from 
  # the interpretation of a frequentest confidence interval
  # more see https://web.stanford.edu/class/stats200/Lecture20.pdf
  
  confidence_upper <- c(confidence_upper, qbeta((1 - alpha / 2), prior_a, prior_b))
  confidence_lower <- c(confidence_lower, qbeta((alpha / 2), prior_a, prior_b))
}

theta_mean <- mean(theta_list)


# sampling ----------------------------------------------------------------
begin <- 1
step <- 50
sample_index <- seq(from=begin, to=length(theta_list), by=step)
theta_list <- theta_list[sample_index]
confidence_upper <- confidence_upper[sample_index]
confidence_lower <- confidence_lower[sample_index]


df1 <- data.frame(observations=c(1:length(theta_list)), theta=theta_list)
df2 <- data.frame(observations=c(1:length(theta_list)), theta_up=confidence_upper, theta_down=confidence_lower)
pic <- ggplot(data = df1, mapping = aes(x = observations, y = theta))+
  geom_line()+labs(x="observations",y="theta")+
  geom_hline(aes(yintercept=0.320039), colour="#BB0000", linetype="dashed")+
  # coord_cartesian(xlim =c(0, ((length(death_list) - begin) / step)), ylim = c(0.308, 1))+
  scale_x_continuous(breaks=c(0,25,50,75,100,125),labels=c(0,25,50,75,100,125)*50,)+
  geom_text(aes(x=100,y=0.4),label=expression(paste(theta,"=0.320039")),cex=6)+
  theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'),legend.position = c(0.15,0.85))+
  geom_ribbon(data = df2, mapping = aes(ymin=theta_down,ymax=theta_up),fill="grey70",alpha=0.3)

pic

# PDF ---------------------------------------------------------------------

set.seed(13579)                                    
N <- 10000                                         
y_rbeta <- rbeta(N, shape1 = prior_a, shape2 = prior_b)        

plot(density(y_rbeta), main = "Posterior Beta Distribution")
