library(corrplot)
library(ggplot2)
library(car)
library(tidyverse)
require(gridExtra)
theme_set(theme_bw(base_size=16))

auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
auto = auto[complete.cases(auto),]

LMOfCars <- function(data){
  lm(mpg~cylinders+
             displacement+
             horsepower+
             weight+
             acceleration+
             auto$model_year+
             factor(origin), 
             data = data,
             na.action = na.exclude)
}

summary(LMOfCars(auto))
# only weight, year, and origin had significant effects

cor_plt <- function(data){
  cor_data <- round(cor(data[, 1:8], use='pairwise.complete.obs'), 3)
  cor_sorted_data <- names(sort(cor_data[, 'mpg'], decreasing = TRUE))
  cor_data <- cor_data[cor_sorted_data, cor_sorted_data]
  
  corrplot.mixed(cor_data, tl.col='black', tl.pos='lt')
}

cor_plt(auto)
# Non-significant factors cylinders, horsepower & displacement were highly correlated with weight

summary(LMOfCars(auto))
# Displacement in regression: -0.02398

plt <- function(a, b, title=''){
  ggplot(auto, aes(x=a, y=b))+
    geom_point()+
    facet_wrap(~factor(origin))+
    stat_smooth(method=lm)+
    labs(x='', y='')+
    ggtitle(title)
}

plt(auto$mpg, auto$displacement, 'mpg v.s. displacement')

# Displacement has the opposite effect in the regression from its visualized effect!

plt(auto$mpg, auto$horsepower, 'mpg v.s. horsepower')
plt(auto$mpg, auto$weight, 'mpg v.s. weight')
plt(auto$mpg, auto$acceleration, 'mpg v.s. acceleration')

# factors like horsepower and weight, seem to have a nonlinear (exponential) relationship with mpg


# Q1
cars <- auto
cars_log <- with(cars, data.frame(log(mpg),
                                  log(cylinders), 
                                  log(displacement),
                                  log(horsepower),
                                  log(weight),
                                  log(acceleration),
                                  model_year,
                                  origin))

names(cars_log) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin")

knitr::kable(head(cars_log))

# a i.
summary(LMOfCars(cars_log))
# horsepower, weight, model_year

# a ii.
# yes, new factors like horsepower, weight, have effects on mpg
# because by loging the entire dataset, we bend the cars dataset in order to fit the regression model

# a iii.
cor_plt(cars_log)
sort(cor(cars_log)[, 'mpg'], decreasing=TRUE)
# skip

# B i.
regr_cars <- summary(LMOfCars(cars))
regr_wt <- lm(mpg~weight, data=cars, na.action=na.exclude)
regr_wt$coefficients
# ii.
regr_cars_log <- summary(LMOfCars(cars_log))
regr_wt_log <- lm(mpg~weight, data=cars_log, na.action=na.exclude)
regr_wt_log$coefficients
# iii.

tibble_density_plot <- function(data1, data2, title='', t1='', t2=''){
  ToDF <- function(x, name=''){
    temp <- list()
    temp$residuals <- x$residuals
    temp$from <- rep(name, length(x$residuals))
    return(data.frame(temp))
  }
  
  d1 <- ToDF(data1, t1)
  d2 <- ToDF(data2, t2)
  temp <- as.tibble(rbind(d1, d2))
  
  temp %>%
    ggplot(aes(x=residuals, fill=from))+
    geom_density(alpha=0.5)+
    geom_vline(xintercept = c(mean(d1$residuals),
                              mean(d2$residuals)),
               col=c('red', 'green'))+
    labs(x='residuals',
         subtitle=title,
         caption='source: auto-data.txt')+
    theme(legend.position = 'bottom')
}

#1.
tibble_density_plot(regr_cars, regr_cars_log, 'Cars', 'cars', 'log cars')
tibble_density_plot(regr_wt, regr_wt_log, 'Weight', 'cars', 'log cars')
tibble_density_plot(regr_wt, regr_cars, 'Weight v.s. Cars', 'weights','cars')
tibble_density_plot(regr_wt_log, regr_cars_log, '(Logged) Weight v.s. Cars','log weight', 'log cars')

#2.
p1 <- ggplot()+
  aes(cars$mpg, resid(regr_wt))+
  geom_point(col="red")+
  stat_smooth(method=lm)+
  geom_hline(yintercept=0, lty='dashed')+
  labs(x='weight', y='residuals')+
  ggtitle('mpg v.s. weight')

p2 <- ggplot()+
  aes(cars_log$mpg, resid(regr_wt_log))+
  geom_point(col="red")+
  stat_smooth(method=lm)+
  geom_hline(yintercept=0, lty='dashed')+
  labs(x='log weight', y='residuals')+
  ggtitle('log.mpg v.s. log.weight')

grid.arrange(p1, p2, ncol=2)

# iv.
# refer to p11 assumptions
# As we can observe from the plots above, the logged regression produces
# a better distributed residuals for the assumptions of regression

# v.
summary(regr_wt)
summary(regr_wt_log)
# the slope of log.mpg v.s. log.weight is much flatter then the mpg vs weight.
# because of the logged process

# c.
# i.
# empty canvas
plot(log(cars$weight), log(cars$mpg), type="n", xlab='weight', ylab='mpg', main='Bootstrapped Confidence Interval')
# function for single resampleed reggresion line
points(log(cars$weight), log(cars$mpg), col="skyblue", pch=19)

boot_regr <- function(model, dataset){
  boot_index <- sample(1:nrow(dataset), replace=TRUE) # random row index number
  data_boot <- dataset[boot_index, ] # picking the rows
  regr_boot <- lm(model, data=data_boot) # run regression model
  abline(regr_boot, lwd=1, col=rgb(0.7, 0.3, 0.3, 0.2), alpha=0.05)
  regr_boot$coefficients
}
coeffs <- replicate(3000, boot_regr(log(mpg)~log(weight), cars))

abline(a = mean(coeffs["(Intercept)", ]),
       b = mean(coeffs["log(weight)", ]),
       lwd=2)

# Confidence interval values
round(quantile(coeffs["log(weight)", ], c(0.025, 0.975)), 3)

# ii.
# traditional way
regression_model <- lm(log(mpg)~log(weight), data=cars)
summary(regression_model)
# estimate +- 1.96*stderr
round(regression_model$coefficients['log(weight)']+c(-1.96, 1.96)*0.0297, 3)
round(confint(regression_model,'log(weight)', level=0.95), 3)

# slightly different results were presented with different computing methods.
# When your data is not decent, bootstrapping will be a more reliable way to compute the regression.

ggplot()+
  aes(coeffs['log(weight)', ])+
  geom_density(fill='skyblue', alpha=0.5)+
  geom_vline(xintercept=mean(coeffs['log(weight)',]), lty='dashed')+
  geom_vline(xintercept=quantile(coeffs['log(weight)', ], c(0.025, 0.975)), col=2, lwd=1.5, lty='dotted')+
  labs(x='log.weight', subtitle = 'Traditional Stat results')

# By finding the slope we get an estimate of the calue by which the dependent variable(mpg) is expected to increase or decrease.
# the Confint provides the range of the slope values that we expect 95% of the times when 
# the sample size is the same. 
# Since obviously neither of the two results includes 0 in their confint, we conclude that there is 
# a significant linear relationship between weight and mpg.

# Q2.
regr_log <- LMOfCars(cars_log)
# diagnosing multicollinearity
weight_regr <- lm(weight~
                    cylinders+
                    displacement+
                    horsepower+
                    acceleration+
                    cars$model_year+
                    factor(origin), 
                  data=cars_log, 
                  na.action = na.exclude)

r2_log_weight <- summary(weight_regr)$r.squared

r2_log_weight
# a.
vif_log_weight <- 1/(1-r2_log_weight)


vif_log_weight
# multicollinearity inflates the variance of the weights by more than 9 times.
sqrt(vif_log_weight) >2
# the high multicollinearity implies that "log weight" shares more than half 
# of its variance with other independent variables.

# b.
# i.
vif_log_cars <- vif(regr_log)
# ii.
sort(vif_log_cars[,'GVIF'], decreasing=TRUE) # generalized VIF
# displacement should be removed
multicollinearity <- function(model, data){
  LM <- lm(model, data)
  sort_order <- sort(vif(LM)[, 'GVIF'], decreasing=TRUE)
  if (unname(sort_order[1] >5 )==TRUE){
    print('Variable you should remove next: ')
    names(sort_order)[1]
  }else{
    print('No more vif of variable is larger than 5.')
    return(LM)
  }

}

multicollinearity(mpg~cylinders+
                    displacement+
                    horsepower+
                    weight+
                    acceleration+
                    cars$model_year+
                    factor(origin), cars_log)

multicollinearity(mpg~cylinders+
                    horsepower+
                    weight+
                    acceleration+
                    cars$model_year+
                    factor(origin), cars_log)

multicollinearity(mpg~cylinders+
                    weight+
                    acceleration+
                    cars$model_year+
                    factor(origin), cars_log)

multicollinearity(mpg~weight+
                    acceleration+
                    cars$model_year+
                    factor(origin), cars_log)
final_Regression_model <- multicollinearity(mpg~weight+
                                              acceleration+
                                              cars$model_year+
                                              factor(origin), cars_log)
summary(final_Regression_model)

summary(LMOfCars(cars_log))
# c.
# yes, we lose horsepower and weight that were previously siginificant.

model_fit <- function(y, yhat){
  variances <- list()
  variances$SSE <- sum((yhat-y)^2)
  variances$SSR <- sum((yhat-mean(y))^2)
  variances$SST <- sum((y-mean(y))^2)
  variances$Rsq <- sum((yhat-mean(y))^2)/sum((y-mean(y))^2)
  return(variances)
}

# about 0.74%
model_fit(cars_log$mpg, regr_log$fitted.values)$Rsq-model_fit(cars_log$mpg, final_Regression_model$fitted.values)$Rsq
summary(regr_log)$r.squared-summary(final_Regression_model)$r.squared

# d.
# i. Ans. 1, when multicollinearity does not exist, standard error would not inflated,
# as a result, the variance of the independent variable shares no variance with other 
# independent variables
# ii.
# vif = 1/(1-r^2) > 5, r > sqrt(4/5)
# indicating that the coefficient of multiple correlation between dependent variable and independent variables should be greater that 0.8
# vif = 1/(1-r^2) > 10, r > sqrt(9/10)
# indicating that the coefficient of multiple correlation between dependent variable and independent variables should be greater that 0.9

# Q3
origin_colors = c("blue", "darkgreen", "red")
with(cars_log, plot(weight, mpg, pch=origin, col=origin_colors[origin]))

plt(cars_log$mpg, cars_log$weight, 'weight v.s. mpg seperated by orgin')

ggplot(cars_log, aes(weight, mpg, color=factor(origin)))+
  geom_point(size=1.5)+
  stat_smooth(method=lm)+
  labs(color='Origin')+
  theme(legend.position = 'bottom')+
  ggtitle('Weight v.s. mpg seperated by origin')+
  guides(color=guide_legend(override.aes = list(size=1.2)))
# I think so, yes.

