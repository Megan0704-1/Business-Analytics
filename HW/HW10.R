if(!exists('foo', mode='function')) source('demo_simple_regression_rsq.R')
interactive_regression_rsq()

scene1_pts <- interactive_regression_rsq()
scene2_pts <- interactive_regression_rsq()
scene3_pts <- interactive_regression_rsq()
scene4_pts <- interactive_regression_rsq()

#Q1
# R-squared is a statistical measure of fit that indicates how much variation of a dependent
# variable is explained by the independent variables in a regression model

# formula : r^2 = 1-unexpected variation/total variation

# a
# I expect the first scenario (1) to have a stronger R^2
# b
# I expect the former scenario (3) to have a stronger R^2

# c
# SST: sum of squares total: 表示y 相對於y bar 的variation
# SSR: sum of squares regression: 表示估計值y hat 新對於中心y bar的variation
# SSE: sum of squares error: 表示variable y相對於estimated value y hat的variation


#Q2
dataset = read.csv('programmer_salaries.txt', sep='\t')
View(dataset)
plot(dataset)

#a.
# Raw data regression
summary(lm(Salary~Experience+Score+Degree, data=dataset))
# R^2: 0.8181
# beta1: how much of the cov of v1 and v2 is explained by the var of v1 (slope)
# beta0: if there were no v1, how much v2 will it be? (intercept)

# Standardized data regression
# for better comparing
summary(lm(Salary~Experience+Score+Degree, data=data.frame(scale(dataset))))# R^2: 0.799
# R^2: 0.8468

lm(Salary~Experience+Score+Degree, data=dataset)$fitted.values[1:5]
lm(Salary~Experience+Score+Degree, data=dataset)$residuals[1:5]

# R is coefficient of multiple correlation between dependent variable and ALL independent variables
# R^2 is the coefficient of determination

#b.
#i.
ones <- rep(1, length(dataset$Experience))
X <- matrix(c(ones, dataset$Experience, dataset$Score, dataset$Degree),
            ncol=4,
            nrow=length(ones),
            byrow=FALSE)

colnames(X) <- c("1","exp","score","degree")
dim(X)
#ii.
y <- as.matrix(dataset$Salary)
dim(y)
#iii.
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y
dim(beta_hat)
print(beta_hat)
#iv
y_hat <- X %*% beta_hat
dim(y_hat)
print(y_hat[1:5,])
residual <- y - y_hat
print(residual[1:5,])
# v
variations <- function(y, y_hat){
  y_mean = mean(y)
  
  stat <- list()
  stat$R_squared <- cor(y, y_hat)^2
  stat$SSR <- sum((y_hat-y_mean)^2)
  stat$SSE <- sum((y-y_hat)^2)
  stat$SST <- sum((y-y_mean)^2)
  
  return(stat)
}

model_fit <- variations(y, y_hat)
round(model_fit$SSR+model_fit$SSE, 2) == round(model_fit$SST, 2)
print(model_fit)

#c.
R_squared_i <- 1-model_fit$SSE/model_fit$SST
R_squared_ii <- model_fit$R_squared
round(R_squared_i, 2) == round(R_squared_ii, 2)
print(c(R_squared_i, R_squared_ii))


#Q3
#a.
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
auto = auto[complete.cases(auto),]
#i.

# create a type features using car_names
type=""
for (name in auto$car_name){
  type = c(type, strsplit(name, split = " ")[[1]][1])
}
auto$type = type[-1]

# visualization
ggplot(data=auto)+
  aes(x = displacement, y=mpg, color=type)+
  geom_point()+
  facet_wrap(.~model_year)


ggplot(data=auto) + 
  geom_bar(mapping=aes(x=origin,fill=factor(origin)))

highchart()%>%
  hc_add_series(auto, "scatter", hcaes(x=weight, y=displacement))
  
highchart()%>%
  hc_add_series(auto, "scatter", hcaes(x=displacement, y=horsepower))

plt <- function(a, b){
ggplot(auto, aes(x=a, y=b))+
  geom_point()+
  stat_smooth(method=lm)+
  labs(x='', y='')
}

plt(auto$model_year, auto$origin)
plt(auto$horsepower, auto$weight)
plt(auto$weight, auto$displacement)
plt(auto$displacement, auto$horsepower)

auto %>% 
  count(type) %>%
  arrange(n) %>%
  hchart(type="bar", hcaes(x=type, y=n))

#ii.
auto <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
cormat <- round(cor(auto[,1:8], use="pairwise.complete.obs"), 2)
View(auto[1:8])
View(cormat)

melted_cormat <- melt(cormat)
knitr::kable(head(melted_cormat))
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+ggtitle('HeatMap')

#iii
#based on the visualization and correlation matrix, I think cylinders, displacement, horsepower, weight show negative correlation with mpg

#iv

cordf <- as.data.frame(cormat)
ggcorrplot(cordf, hc.order = TRUE, type = "lower",
           lab = TRUE)

#v.
cormat[cormat<=0.7|cormat==1] <- ''
knitr::kable(head(cormat))
# as we can observe from the plot above, 
# displacement has a highly positive correlation with cylinders, weight, horsepoewer
# cyinders has a highly positive correlation with weight and cylinders
# weight has a highlt positive correlation with horsepower

# mpg has a highly negative correlation with displacement, cylinders, weight and horsepower

#b.
summary(lm(mpg~cylinders+
             displacement+
             horsepower+
             weight+
             acceleration+
             model_year+
             factor(origin), data=auto[1:8]))
#i.
#cylinders, horsepower, acceleration

#ii.
# Standardizing data for comparing in the same units
# no, before standardizing your data, we are getting results that measureing slopes, intercepts by
# different units. Comparing values based on different units is meaningless.

#c.
#i.
summary(lm(mpg~cylinders+
             displacement+
             horsepower+
             weight+
             acceleration+
             model_year, data=as.data.frame(scale(auto[1:7]))))
# yes, ofcourse.
# as we can observe from the results, by reducing weights of a vehicle, mpg would be able to increase effectively.

#ii.
# weight
summary(lm(mpg~weight, data=as.data.frame(auto[c('mpg','weight')])))
# model_year
summary(lm(mpg~model_year, data=as.data.frame(auto[c('mpg','model_year')])))
# both of them become significant after regressing mpg over them

#iii.
regr <- lm(mpg~cylinders+
             displacement+
             horsepower+
             weight+
             acceleration+
             model_year+
             factor(origin), data=auto[1:8])$residuals
ggplot()+
  aes(regr)+
  geom_density()+
  ggtitle('Residual Density Plot')+
  geom_vline(xintercept = 0, colour='red', lwd=1.2)+
  geom_vline(xintercept = c(sd(regr), -sd(regr)), colour='red', alpha=0.3)

ggplot()+
  aes(regr)+
  geom_boxplot()

#yes.

