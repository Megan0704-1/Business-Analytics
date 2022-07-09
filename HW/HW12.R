library(car)
library(ggplot2)
library(corrplot)
library(dplyr)
library(tidyverse)
require(gridExtra)

theme_set(theme_bw(base_size=16))

auto <- read.table('auto-data.txt', header=FALSE, na.strings = '?')
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
auto = with(auto, cbind(mpg, weight, acceleration, model_year, origin))
auto = as.data.frame(auto[complete.cases(auto),])

cars_log <- with(auto, data.frame(log(mpg),
                                  log(weight),
                                  log(acceleration),
                                  auto$model_year,
                                  factor(auto$origin)))            

View(cars_log)
names(cars_log) <- c("mpg", "weight",
                     "acceleration", "model_year", "origin")
knitr::kable(cars_log)

#Q1
#a.
#i.moderate
light_wg_cars <- cars_log %>% subset(weight<mean(weight))
heavy_wg_cars <- cars_log %>% subset(weight>=mean(weight))
# Simple example for do the log transformation on the original data first.

# Say you have 3 measurements with values of 1, 10, and 100.
# Their mean value is 111/3=37. The base 10 logarithm of 37 is 1.57, which is the log of their mean value in the original scale.
# with the base 10 logarithms of the original data are 0, 1, and 2; the mean of the logarithms is 1, corresponding to a value of 10 in the original scale.

# As a result, mean of the log does not equal the log of the mean.
# so if the log transformation of the data is appropriate,
# always do the transformation on the original data first.

# ii.
p1 <- ggplot(light_wg_cars)+
  geom_point(aes(acceleration, mpg, color=weight))+
  stat_smooth(aes(acceleration, mpg), method=lm)+
  ggtitle('Light weight cars')
  
p2 <- ggplot(heavy_wg_cars)+
  geom_point(aes(acceleration, mpg, color=weight))+
  stat_smooth(aes(acceleration, mpg), method=lm)+
  ggtitle('Heavy weight cars')

grid.arrange(p1, p2)
# putting them together
weight_cls <- as.numeric(cars_log[,'weight']>mean(cars_log$weight))
cars_log <- cbind(cars_log, weight_cls)
# Heavy:1, light:0

ggplot(cars_log)+
  geom_point(aes(x=acceleration, y=mpg, color=weight_cls))+
  stat_smooth(aes(y=mpg, x=acceleration), method=lm)

#iii.
ggplot(cars_log, aes(acceleration, mpg, color=factor(weight_cls)))+
  geom_point(size=1.5)+
  labs(color='weight_cls')+
  stat_smooth(method=lm)+
  theme(legend.position = 'bottom')+
  ggtitle('Acceleration v.s. MPG seperated by weight')+
  guides(color=guide_legend(override.aes = list(size=1.2)))

# b.
# light
summary(with(light_wg_cars, lm(mpg~weight+acceleration+model_year+factor(origin))))
# heavy
summary(with(heavy_wg_cars, lm(mpg~weight+acceleration+model_year+factor(origin))))
   
# acceleration is depends on the weight!! 
# the heavier the car is, the higher acceleration value.
     
# By observing the R^2 values, I deducted that
# vehicles that weigh heavier tend to be more related to the target explanatory variable mpg.

#Q2 moderation

# a.
# Guessing from my inexperienced intuition, I state that the weight
# might be a moderator of accerleration affecting mileage.

# b.
# drop the weight class
cars_log = cars_log[,-length(cars_log)]

# Identifying symptoms of multicollinearity

#i.
summary(with(cars_log, lm(mpg~weight+acceleration+model_year+factor(origin))))
# for the average car

#ii.
interaction_term <- with(cars_log, weight*acceleration)
summary(with(cars_log, lm(mpg~weight+acceleration+model_year+factor(origin)+interaction_term)))
# this is a basic sign of multicollinearity
# p value is messed up.
# if interaction term is added, acceleration term grows dramatically.

#iii.
# check this one
cars_log$origin <- as.numeric(cars_log$origin)

# scale argument inside means you only proceed mean-centered
# not devided by the sd.

# FS regression with interaction
log_weight_mc = scale(cars_log$weight, scale=FALSE)
log_acc_mc = scale(cars_log$acceleration, scale=FALSE)
interaction_term = log_weight_mc * log_acc_mc
with(cars_log, cor(log_weight_mc, interaction_term))
with(cars_log, cor(log_acc_mc, interaction_term))

summary(lm(mpg~
             weight+
             acceleration+
             interaction_term+
             model_year+
             factor(origin),
           data = cars_log))
# use the original logged data
# instead of the scaled one!!

# note that we can not statistically remove multicollinearity

# as we can observe from the summary table, 
# there is no change in significance or R^2 value
# thus, we can only improve interpretability of coeffs


# iv. orthogonal
interaction <- with(cars_log, lm(mpg~weight+acceleration+interaction_term+model_year+factor(origin)))
orthogonal_term <- interaction$residuals                         
summary(with(cars_log, lm(mpg~
                            weight+
                            acceleration+
                            model_year+
                            factor(origin)+
                            orthogonal_term)
             )
        )
# Original dataset
summary(with(cars_log, lm(mpg~
                            weight+
                            acceleration+
                            model_year+
                            factor(origin)
                          )
             )
)
# Despite the fact that the estimating value looks almost the same as the original data,
# as we can observe from the summary table, the standard error did slightly decrease.
# as a result, we can conclude that orthogonalization gives us the most interpretable coeff.
# however, it still does not statistically remove multicollinearity.

# c.
cor_plt <- function(data){
  cor_data <- round(cor(data[, 1:length(data)], use='pairwise.complete.obs'), 3)
  cor_sorted_data <- names(sort(cor_data[, 'mpg'], decreasing = TRUE))
  cor_data <- cor_data[cor_sorted_data, cor_sorted_data]
  
  corrplot.mixed(cor_data, tl.col='black', tl.pos='lt')
}

# calculate cor between weight, acceleration with interactive term respectively.
cor(cars_log$weight, interaction_term)
cor(cars_log$acceleration, interaction_term)
# it seems that acceleration has a higher correlation with the interactive term.

# raw:
cor(cars_log)
cor_plt(cars_log)

# interaction(weight*acc):
with_interaction <- cbind(cars_log, 'interaction'=cars_log$weight*cars_log$acceleration)
cor(with_interaction)
cor_plt(with_interaction)
summary(lm(mpg~weight+acceleration+interaction, data=with_interaction))

# mean-centered:
scaled_w <- scale(cars_log$weight, center=TRUE, scale=FALSE)
scaled_a <- scale(cars_log$weight, center=TRUE, scale=FALSE)
scaled_i <- scaled_w * scaled_a
cor(scaled_w, scaled_i) # == cor()
 # fully standardized version
summary(lm(mpg~weight+acceleration+interaction, data=as.data.frame(scale(with_interaction, center=TRUE, scale=FALSE))))
# as we can observe from the above summary table,
# it does not matter whether you scale the data or not you get the same R^2 value
# However, it does decrease the stderr value dramaticlly.

# FS cor
with(as.data.frame(scale(cars_log)), cor(weight, weight*acceleration))
with(as.data.frame(scale(cars_log)), cor(acceleration, weight*acceleration))


# orthogonal
car_log_regr <- lm(mpg~weight+acceleration+interaction_term, data=auto)
with_orthogonal <- cbind(cars_log, 'orthogonal'=car_log_regr$residuals)
cor(with_orthogonal)
cor_plt(with_orthogonal)
summary(lm(mpg~weight+acceleration+orthogonal, data=with_orthogonal))

#Note that this method does not eliminate the multicillinearity either.

# Q3

# check this one too
# cylinder term is highly correlated to weight -> multicollinearity

#a.
auto <- read.table('auto-data.txt', header=FALSE, na.strings = '?')
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
auto = with(auto, cbind(mpg, weight, acceleration, model_year, origin, cylinders))
auto = as.data.frame(auto[complete.cases(auto),])

cars_log <- with(auto, data.frame(log(mpg),
                                  log(weight),
                                  log(acceleration),
                                  auto$model_year,
                                  auto$origin,
                                  log(cylinders)))            

names(cars_log) <- c("mpg", "weight","acceleration", 
                     "model_year", "origin", 'cylinders')

cor(cars_log)
# we can tell from the correlation matrix that weight and cylinders are highly correlated.

model_1 <- lm(weight~cylinders, data=cars_log)
summary(model_1)

###### model2 should include every factor(instead of cylinders) to run the direct effect on the regression model!!!!!!
model_2 <- lm(mpg~weight+
                acceleration+
                model_year+
                factor(origin)
              , data=cars_log)
summary(model_2)
#b.
#indirected effect
model_1$coefficients[2] * model_2$coefficients[2]

#c.
# bootstrapping the significance effect of the indirected effect

boot_model <- function(model1, model2, dataset){
  boot_index <- sample(1:nrow(dataset), replace=TRUE)
  new_data <- dataset[boot_index,]
  regr1 <- lm(model1, new_data)
  regr2 <- lm(model2, new_data)
  return(regr1$coefficients[2]*regr2$coefficients[2])
}

indirect <- replicate(2000, boot_model(model_1, model_2, cars_log))
quantile(indirect, probs=c(0.025, 0.975))
plot(density(indirect))
abline(v = quantile(indirect,
                    probs=c(0.025, 0.975)),
       col='red',
       lty='dashed')


ggplot()+
  aes(indirect)+
  geom_density()+
  geom_vline(xintercept=quantile(indirect, c(0.025, 0.975)), col='red', lty=2)+
  ggtitle('Distribution of 95% CI of the indirect effect')

# Visualization
library(lattice)
library(manipulate)

# creating x, y grid of all possible points
g <- expand.grid(weight=6:10, acceleration = seq(1.5, 3.5, 0.5))
auto_regr <- lm(mpg~weight+acceleration, data=auto)
g$mpg <- predict(auto_regr, g)
auto_regr_intxn <- lm(mpg~weight+acceleration+weight*acceleration, data=auto)
g$mpg <- predict(auto_regr_intxn, g)

wireframe(mpg~weight*acceleration, data=g, zlim=c(27, 35),
          scales=list(arrows=FALSE), drape=TRUE, colorkey=FALSE,
          screen=list(z=40, x=-70))

require(manipulate)
manipulate(
  wireframe(mpg~weight*acceleration, data=g, zlim=c(27, 35),
            scales=list(arrows=FALSE), drape=TRUE, colorkey=FALSE,
            screen=list(z=z_rot, x=x_rot)),
  z_rot = slider(0, 360, initial=40),
  x_rot = slider(-90, -70, initial = -70)
)
