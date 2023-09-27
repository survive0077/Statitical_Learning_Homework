library(ggplot2)


df <- read.csv('./mydata.csv')
variable_list <- c("hospital_expire_flag", "gender", "admission_age")
list_1 <- df[variable_list]
obs_num <- dim(df)[1]


# Q1 Fig1 -----------------------------------------------------------------

F_sum <- 0
M_sum <- 0
F_ddnum <- 0
M_ddnum <- 0
for(i in 1:obs_num)
{
  if(list_1$gender[i] == "F")
  {
    F_sum <- F_sum + 1
    if(list_1$hospital_expire_flag[i])
      F_ddnum <- F_ddnum + 1
  }
  else
  {
    M_sum <- M_sum + 1 
    if(list_1$hospital_expire_flag[i])
      M_ddnum <- M_ddnum + 1
  }
}
F_ddratio <- round(F_ddnum / F_sum, 2)
M_ddratio <- round(M_ddnum / M_sum, 2)

d <- data.frame(state = c(0,1,0,1), gender = c("F","F","M","M"), observations = c(F_sum-F_ddnum, F_ddnum, M_sum-M_ddnum, M_ddnum)/ 1000)
d <- cbind(d, ratio=c(F_ddratio, F_ddratio, M_ddratio, M_ddratio))
d$state <- factor(d$state,level=c(1,0))

ggplot(data=d,aes(x=gender,y=observations),fill=state)+
  geom_col(aes(x=gender, y=observations,fill=state))+
  labs(x='sex',y='observations(x10^3)')+
  scale_fill_discrete(labels=c(1,0))+
  theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'),legend.position = c(0.15,0.85))+
  geom_line(aes(x=gender,y=ratio*6),group=2,size=1.2)+
  geom_point(aes(x=gender,y=ratio*6,size=1.2),show.legend=FALSE)+
  geom_text(aes(x=gender,y=ratio*6+0.2),label=d$ratio,cex=5)+
  scale_y_continuous(name ='observations',sec.axis = sec_axis(~./6,name='ratio'))


# Q1 Fig2 -----------------------------------------------------------------
interval <- 5
age_sum <- vector()
age_ddnum <- vector()
for(i in 1:obs_num)
{
  index = (list_1$admission_age[i] + interval / 2) %/% interval + 1
  if(is.na(age_sum[index]))
  {
    age_sum[index] <- 1
    age_ddnum[index] <- 0
  }
  else
    age_sum[index] <- age_sum[index] + 1
  if(list_1$hospital_expire_flag[i])
    age_ddnum[index] <- age_ddnum[index] + 1
}
age_radio <- round(age_ddnum / age_sum, 2)
max_length <- length(age_sum)

na_num <- sum(is.na(age_sum))
age_sum <- age_sum[(na_num+1):max_length]
age_radio <- age_radio[(na_num+1):max_length]
age_ddnum <- age_ddnum[(na_num+1):max_length]

age_intvlnum <- max_length - na_num
ageintvl <- c(na_num:(max_length-1)) * interval
state <- c(rep(c(0),age_intvlnum),rep(c(1),age_intvlnum))
d1 <- data.frame(state=state, age_intvl=rep(ageintvl,2), observations=c((age_sum-age_ddnum),age_ddnum) / 100, ratio=rep(age_radio,2))
d1$state <- factor(d1$state,level=c(1,0))

ggplot(data=d1,aes(x=age_intvl,y=observations),fill=state)+
  geom_col(aes(x=age_intvl, y=observations,fill=state))+
  scale_fill_discrete(labels=c(1,0))+
  labs(x='age',y='observations(x10^2)')+
  theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'),legend.position = c(0.15,0.85))+
  geom_line(aes(x=age_intvl,y=ratio*10),group=2,size=1.2)+
  geom_point(aes(x=age_intvl,y=ratio*10,size=1.2),show.legend=FALSE)+
  geom_text(aes(x=age_intvl,y=ratio*10+0.5),label=d1$ratio,cex=4)+
  scale_y_continuous(name ='observations',sec.axis = sec_axis(~./10,name='ratio'))
