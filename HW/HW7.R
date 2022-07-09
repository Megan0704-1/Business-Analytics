library(ggplot2)
library(dplyr)
library(tidyr)
#runif(1)*10^9
set.seed(453574558)

media1 <- read.csv('pls-media1.csv')
media2 <- read.csv('pls-media2.csv')
media3 <- read.csv('pls-media3.csv')
media4 <- read.csv('pls-media4.csv')

media1 <- data.frame(media1$media, media1$INTEND.0)
colnames(media1) <- c('Media1', 'Intend1')
media2 <- data.frame(media2$media, media2$INTEND.0)
colnames(media2) <- c('Media2', 'Intend2')
media3 <- data.frame(media3$media, media3$INTEND.0)
colnames(media3) <- c('Media3', 'Intend3')
media4 <- data.frame(media4$media, media4$INTEND.0)
colnames(media4) <- c('Media4', 'Intend4')

#a
mean(media1$Intend1)
mean(media2$Intend2)
mean(media3$Intend3)
mean(media4$Intend4)

#b
label <- c( "media 1"='red', 
            "media 2"='cornflowerblue',
            "media 3"='goldenrod1', 
            "media 4"='chartreuse3')
plt <- ggplot() + 
  ggtitle('Density Plot')+
  geom_density(aes(media1$Intend1)) + 
  stat_density(aes(media1$Intend1),color="red", geom="line",position="identity", lwd=1.2)+
  geom_density(aes(media2$Intend2)) + 
  stat_density(aes(media2$Intend2),color="cornflowerblue", geom="line",position="identity", lwd=1.2) +
  geom_density(aes(media3$Intend3)) + 
  stat_density(aes(media3$Intend3),color="goldenrod1", geom="line",position="identity", lwd=1.2) +
  geom_density(aes(media4$Intend4)) + 
  stat_density(aes(media4$Intend4), color='chartreuse3',geom="line",position="identity", lwd=1.2)

plt+geom_vline(xintercept = mean(media1$Intend1), color='red', lwd=1)+
  geom_vline(xintercept = mean(media2$Intend2), color='cornflowerblue', lwd=1)+
  geom_vline(xintercept = mean(media3$Intend3), color='goldenrod1', lwd=1)+
  geom_vline(xintercept = mean(media4$Intend4), color='chartreuse3', lwd=1)+
  labs(x='', y='Density')+
  scale_fill_manual(values=label, aesthetics = c("colour", "fill"))+
  theme(
    legend.position=c(0.2,0.8),
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  )

# transfer into long data
long_media1 <- gather(as.data.frame(media1$Intend1))
long_media2 <- gather(as.data.frame(media2$Intend2))
long_media3 <- gather(as.data.frame(media3$Intend3))
long_media4 <- gather(as.data.frame(media4$Intend4))

View(all_media)
long_media1$key <- 'm1'
long_media2$key <- 'm2'
long_media3$key <- 'm3'
long_media4$key <- 'm4'

all_media <- rbind(long_media1, long_media2, long_media3, long_media4)

all_media$key <- as.factor(all_media$key)
plt <- ggplot(all_media, aes(x=key, y=value, fill=key))+
  geom_boxplot()+
  coord_flip()+
  scale_x_discrete(limits=rev(levels(all_media$key)))

plt+
  ggtitle('Boxplot')+
  labs(x='Media', y='Value')+
  theme(
    legend.position = 'right',
    legend.text = element_text()
  )+
  guides(fill=guide_legend(reverse=FALSE))

# c
# Observe from the plots presented above, we can infer that media two may be slightly different from the others,

#Q2
# a
# H0: m1=m2=m3=m4

# b (i)
media_values <- split(all_media$value, f=all_media$key)
lengths <- sapply(media_values, length)
lengths

MSTR <- function(all_values){
  sstr=0
  
  grandmean <- mean(all_media$value)
  df_mstr <- length(all_values)-1
  
  for(i in c(1:(df_mstr+1))){
    n = length(all_values[[i]])
    xtilde = mean(all_values[[i]])
    sstr = sstr+n*(xtilde-grandmean)^2
    #print(sstr)
  }
  return(data.frame('df_mstr' = df_mstr,
                    'mstr' = round(sstr/df_mstr, 2)))
}

MSE <- function(all_values){
  sse=0
  
  nT <- length(all_media$value)
  k <- length(all_values)
  df_mse <- nT-k
  
  for(i in c(1:k)){
    n = length(all_values[[i]])
    #print(n)
    Sj = var(all_values[[i]])
    sse = sse+(n-1)*Sj
  }
  
  return(data.frame('df_mse' = df_mse,
                    'mse' = round(sse/df_mse, 2)))
}

mstr_value <- MSTR(media_values); mstr_value
mse_value <- MSE(media_values); mse_value
Fvalue <- mstr_value$mstr/mse_value$mse; round(Fvalue, 3)

# (ii)
qf(p=0.95, df1 <- mstr_value$df_mstr, df2 <- mse_value$df_mse)
Pvalue <- pf(Fvalue, mstr_value$df_mstr, mse_value$df_mse, lower.tail = FALSE)
Pvalue > 1-0.95
round(Pvalue, 4)

#since the Pvalue is slightly larger than the significance level, we state that we do not have enough confidence to reject the null hypothesis.

# c


oneway.test(all_media$value~factor(all_media$key), var.equal=TRUE)
summary(aov(all_media$value~factor(all_media$key)))

# d: Tukey Test

anova_model <- aov(all_media$value~factor(all_media$key))
TukeyHSD(anova_model, conf.level = 0.05)

# If the assumptions hold, as we can observe from the summary, the lower different values between the means, the higher the p-values are.
#all pairs of media have significant different means.
# especially between m1-m3, m1-m4, and m3-m4, which indicates that these pairs might not be different from each other.
# In conclusion, it seems that we can not reject the null hypothesis.

#e

# ANOVA requires some assumption to be met.
# each treatments ot populations response variable is normally distributed.
# the variance of the response variables is the same for all treatments or populations.
# the response variables are not related between groups

# So I decide to verify if the classic requirements of one-way ANOVA were met by conducting Dunn Test.
# Dunn Test
library(FSA)
dunnTest(all_media$value~factor(all_media$key), data=all_media, method='bonferroni')
# when there is no assumptions of variance homogeneity and no assumptions of equal group sizes, 
# we can easily observe from the table that almost no pairs of media have significant different means except for 
# m2 and m4. their p-value lies slightly below the significance level: 0.05
# this result verify that the classic requirements of one-way ANOVA are met.


#Q3 Kurskal Wallis
View(all_media)
all_media$rank <- rank(all_media$value)
group_rank <- split(all_media$rank, f=all_media$key)
sapply(group_rank, sum) 

H <- function(all_values){
  h=0
  N = length(all_media$value)
  k = length(all_values)
  for(i in 1:k){
    R = sum(group_rank[[i]])
    n = length(group_rank[[i]])
    h = h+R^2/n
  }
  return(12/(N*(N+1))*h-3*(N+1))
}

H_value <- H(media_values); H_value
kw_p <- round(1-pchisq(H_value, df=3), 5); kw_p<1-0.95; kw_p
# Based on the result we get, H value is significant, thus we can conclude that we have enough evidence to reject the null hypothesis, that is to say, not all the values among the groups are the same.

kruskal.test(all_media$value~factor(all_media$key), data=all_media)

# There will be some difference in the calculation of H because `kurskal.test()` accounts for ties in rank.
# However, we still got the similar results, not all the values among the groups are the same.

#d
dunnTest(all_media$value~factor(all_media$key), data=all_media, method='bonferroni')
# we can conclude that nearly all 4 media formats have similar results except for m2 and m4.