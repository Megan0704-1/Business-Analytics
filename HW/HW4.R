#we call the Z-score of an app's retention rate a DOI score.
#The DOI score indicates an app has a statistically significant lower retention rate if the Z-score is much less than -3.7.
standardize <- function(data){
  return((data-mean(data))/sd(data))
}

install.packages(ggplot2)
install.packages(gapminder)
library(ggplot2)
library(gapminder)
library(gridExtra)

#2
claims <- 7.6
repair_times <- read.csv("verizon.csv")
TIME <- repair_times$Time
Group <- repair_times$Group
ILEC <- repair_times[repair_times$Group=="ILEC",]
CLEC <- repair_times[repair_times$Group=="CLEC",]

#(a) (i)
p1 <- ggplot(repair_times, aes(x=Time))+geom_density()+geom_vline(aes(xintercept = mean(Time)), color="blue", linetype="dashed", size=1)+ggtitle("Repair Time")
p2 <- ggplot(ILEC, aes(x=Time))+geom_density()+geom_vline(aes(xintercept = mean(Time)), color="blue", linetype="dashed", size=1)+ggtitle("ILEC")
p3 <- ggplot(CLEC, aes(x=Time))+geom_density()+geom_vline(aes(xintercept = mean(Time)), color="blue", linetype="dashed", size=1)+ggtitle("CLEC")
grid.arrange(p1, p2, p3, nrow=1)
# (ii)
# (iii)
pop_mean <- mean(TIME)
pop_sderr <- sd(TIME)/sqrt(length(TIME))
pop_mean + c(-1, 1)*2.56*sd(TIME)/pop_sderr
# (iv)
sample_sd <- function(data){
  return(sqrt(sum((data-mean(data))^2)/(length(data)-1)))
}

claims <- 7.6
repair_times <- read.csv("verizon.csv")
sample_size <- length(TIME)
sample_mean <- mean(TIME)
sample_sd <- sample_sd(TIME)
sderr <- sample_mean/sqrt(sample_size)
t <- (sample_mean-claims)/sderr
t
p <- (1-pt(t, sample_size-1))
p
# Very unlikely that the Verizon is right, plz review on the hypothesis test

# (b)
set.seed(runif(1)*10^9)
boot_mean_diff <- function(data, claim){
  resample <- sample(data, length(data), replace = TRUE)
  return(mean(resample)-claim)
}

hyp_mean_diff <- replicate(2000, boot_mean_diff(TIME, claims))
hyp_ci_99 <- quantile(hyp_mean_diff, probs=c(0.025, 0.975))
pop_mean_diff <- replicate(2000, boot_mean_diff(TIME, mean(TIME)))
pop_ci_99 <- quantile(pop_mean_diff, probs=c(0.025, 0.975))

cat("pop: ",pop_ci_99,"\nhyp: ", hyp_ci_99)
pop_ci_99-hyp_ci_99

#=============
boot_t_stat <- function(data, claim){
  resample <- sample(data, length(data), replace=TRUE)
  mean_diff <- mean(resample)-claim
  se <- sample_sd(resample)/sqrt(length(mean_diff))
  return(mean_diff/se)
}

random <- runif(1)*10^9
set.seed(random)
t_boots <- replicate(2000, boot_t_stat(TIME, claims))
mean(t_boots)

par(mfrow=c(1,1))
plot(density(t_boots), col="blue", lwd=2)
t_boot_ci_99 <- quantile(t_boots, c(0.05, 0.995))
t_pop_ci_99 <- quantile(TIME, c(0.05, 0.995))

abline(v = mean(t_boots))
abline(v = t_ci_99, lty="dashed")

ggplot()+aes(pop_mean_diff)+geom_density()+geom_vline(xintercept = mean(pop_mean_diff), color="blue")

library(ggpubr)
p1 <- ggplot(repair_times, aes(x=Time))+geom_density()+geom_vline(aes(xintercept = mean(Time)), color="blue", linetype="dashed", size=1)+xlim(-5, 75)
p2 <- ggplot(ILEC, aes(x=Time))+geom_density()+geom_vline(aes(xintercept = mean(Time)), color="blue", linetype="dashed", size=1)+xlim(-5, 75)
p3 <- ggplot(CLEC, aes(x=Time))+geom_density()+geom_vline(aes(xintercept = mean(Time)), color="blue", linetype="dashed", size=1)+xlim(-5, 75)
ggarrange(p2, p3, p1, labels=c("ILEC", "CLEC", "Repaired_time"), ncol=2, nrow=2, vjust=1, hjust=0)

sample_sd <- function(data){
  v <- sum((data-mean(data))^2)/(length(data)-1)
  return(sqrt(v))
  
}
sample_sd(TIME)
sd(TIME)


c_mean <- mean(TIME)   #樣本平均數
c_sd <- sd(TIME)        #給定標準差
n <- length(TIME)         #樣本

plot(x=c(7,10),  #劃出一個空白兩個維度的框架
     y=c(0,110),
     type = "n")    #不畫任何點或線

abline(v = c_mean,  #劃出平均數位置的線
       lty = 2,     #選擇虛線
       col = "red") #選擇紅色


for(i in 1:100){
  x = rnorm(2000 , c_mean , c_sd)           #隨機產生一組以(n,c_mean,c_sd)為參數
  
  width = qt(0.995 , n-1)*sd(x)/sqrt(n)  #計算95%信賴區間估計誤差的界線(bound on the error of estimation)
  
  
  left = mean(x)-width                   #信賴區間下限(Lower confidence limit) 
  right = mean(x)+width                  #信賴區間上限(Upper confidence limit) 
  
  if (c_mean >= left && c_mean <= right) #抽樣結果落在區間內
  { lines(c(left,right),                 #X軸位置連接上下限
          c(i,i),                        #Y軸位置連接上下限
          lty = 2)                       #選擇虛線                  
  }
  else                                   #抽樣結果不落在區間內
  {
    lines(c(left,right),                 #X軸位置連接上下限
          c(i,i),                        #Y軸位置連接上下限
          lty = 1,                       #選擇實線
          lwd = 2,
          col="green")                       #選擇兩倍線寬
  }
  
}

x <- rnorm(100, mean=50, sd=2)
ci <- quantile(x, probs=c(0.005, 0.995))
plot(density(x))
abline(v = ci, col="red")
ci
ci <- mean(x)+c(-1, 1)*2.56*sd(x)/sqrt(100)


boot_mean <- function(data, fun){
  resample <- sample(data, length(data), replace=TRUE)
  fun(resample)
}

library(ggplot2)
replicate(2, boot_mean(TIME, mean))
ggplot()+aes(boot_t)+geom_density()+geom_vline(xintercept = quantile(boot_t, c(0.005, 0.995)), color="red")
install.packages(c("ggplot2", "gapminder", "gridExtra", "ggpubr"))