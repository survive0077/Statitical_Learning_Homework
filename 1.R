library(ggplot2)
library(ggpubr)

df <- read.csv('./mydata.csv')

# function ----------------------------------------------------------------

function_continuous_process <- function(interval=10, dolist, name, if_3sigma=FALSE) 
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
  
  for(i in 1:obs_num)
  {
    index = (do_df$target[i] + interval / 2) %/% interval + 1
    if(is.na(vec_sum[index]))
    {
      vec_sum[index] <- 1
      vec_ddsum[index] <- 0
    }
    else
      vec_sum[index] <- vec_sum[index] + 1
    
    if(do_df$ifdead[i])
      vec_ddsum[index] <- vec_ddsum[index] + 1
  }
  
  vec_radio <- round(vec_ddsum / vec_sum, 2)
  na_index <- which(is.na(vec_ddsum))
  vec_ddsum[na_index] <- 0
  vec_radio[na_index] <- 0
  max_length <- length(vec_sum)
  
  c_na_num <- which(is.na(vec_sum))
  # max?
  na_num <- max(c_na_num)
  vec_sum <- vec_sum[(na_num+1):max_length]
  vec_radio <- vec_radio[(na_num+1):max_length]
  vec_ddsum <- vec_ddsum[(na_num+1):max_length]
  
  vec_intvlnum <- max_length - na_num
  vec_intvl <- c(na_num:(max_length-1)) * interval
  
  state <- c(rep(c(0),vec_intvlnum),rep(c(1),vec_intvlnum))
  d <- data.frame(state=state, 
                  intvl=rep(vec_intvl,2), 
                  observations=c((vec_sum-vec_ddsum),vec_ddsum) / 100, 
                  ratio=rep(vec_radio,2))
  d$state <- factor(d$state,level=c(1,0))
  
  pic <- ggplot(data=d)+
    geom_col(aes(x=intvl, y=observations,fill=state))+
    scale_fill_discrete(labels=c(1,0))+
    labs(x=name,y='observations(x10^2)')+
    theme(panel.grid=element_blank(), panel.background=element_rect(fill='transparent', color='black'),legend.position = c(0.15,0.85))+
    geom_line(aes(x=intvl,y=ratio*10),group=2,size=1.2)+
    geom_point(aes(x=intvl,y=ratio*10,size=1.2),show.legend=FALSE)+
    geom_text(aes(x=intvl,y=ratio*10+0.5),label=d$ratio,cex=4)+
    scale_y_continuous(name ='observations(x10^2)',sec.axis = sec_axis(~./10,name='ratio'))
  
  return(pic)
}

function_discrete_process <- function(dolist, name) 
{
  counts <- table(dolist)
  dd_counts <- table(dolist,df$hospital_expire_flag==1)
  item_name <- names(counts)
  ratio = round(dd_counts[,"TRUE"] / counts ,2)
  
  d <- data.frame(state = c(rep(0,length(item_name)),rep(1,length(item_name))), 
                  characteristic = rep(item_name,2), 
                  observations = c(dd_counts[,"FALSE"],dd_counts[,"TRUE"]) / 1000,
                  ratio = rep(ratio,2))
  d$state <- factor(d$state,level=c(1,0))
  
  pic <- ggplot(data=d,aes(x=characteristic,y=observations),fill=state)+
            geom_col(aes(x=characteristic, y=observations,fill=state))+
            labs(x=d$name,y='observations(x10^3)')+
            scale_fill_discrete(labels=c(1,0))+
            theme(axis.text.x = element_text(size = 8, angle = 10, hjust = 1), panel.grid=element_blank(), 
                  panel.background=element_rect(fill='transparent', color='black'),legend.position = c(0.15,0.85))+
            geom_line(aes(x=d$characteristic,y=d$ratio*6),group=2,size=1.2)+
            geom_point(aes(x=d$characteristic,y=d$ratio*6,size=1.2),show.legend=FALSE)+
            geom_text(aes(x=d$characteristic,y=d$ratio*6+0.2),label=d$ratio,cex=5)+
            scale_y_continuous(name ='observations(x10^3)',sec.axis = sec_axis(~./6,name='ratio'))
  
  return (pic)
}

# Q1 Fig1 -----------------------------------------------------------------

pic1 <- function_discrete_process(df$gender, name="sex")
pic1
# Q1 Fig2 -----------------------------------------------------------------

pic2 <- function_continuous_process(interval = 5, dolist = df$admission_age, name = "age", if_3sigma = FALSE)
pic2


# Q1 Fig3 -----------------------------------------------------------------

pic3 <- function_continuous_process(interval = 10, dolist = df$weight_admit, name = "weight", if_3sigma = TRUE)
pic3


# Q1 Fig4 -----------------------------------------------------------------

pic4 <- function_continuous_process(interval = 5, dolist = df$height, name = "height", if_3sigma = TRUE)
pic4


# Q1 Fig5 -----------------------------------------------------------------

pic5 <- function_discrete_process(df$ethnicity, name="ethnicity")
pic5

ggarrange(pic1,pic2,pic3,pic4,pic5,
          lables=c("A", "B", "C", "D", "E"),
          ncol = 2,nrow=3)
