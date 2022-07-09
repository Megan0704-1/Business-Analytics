library(ggplot2)
library(grid)
library(devtools)
library(ggbiplot)
library(factoextra)
library(tidyverse)
library(magrittr)
library(FactoMineR)
library(corrplot)


cor_plt <- function(data){
  cor_data <- round(cor(data[, 1:length(data)], use='pairwise.complete.obs'), 3)
  corrplot.mixed(cor_data, tl.col='black', tl.pos='lt')
}

auto = read.table('data/auto-data.txt', header=FALSE, na.strings = '?')
names(auto) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")
auto = as.data.frame(auto[complete.cases(auto),])

car_log = with(auto, data.frame(log(mpg),
                                log(cylinders),
                                log(displacement),
                                log(horsepower),
                                log(weight),
                                log(acceleration),
                                model_year,
                                origin))
car_log = as.data.frame(car_log[complete.cases(car_log),])
cor_plt(car_log)

# 1.a.i
high_corr_variables = with(car_log, data.frame(log.cylinders.,
                                               log.displacement.,
                                               log.horsepower.,
                                               log.weight.)
)

knitr::kable(high_corr_variables)
plot(high_corr_variables)

# what is pca?
# pca is a dimensionality reduction method often used to reduce the dimensionalty of
# large data sets, by transforming a large set of variables into a smaller one that
# still contains most of the information in the large set.

# reducing the number of variables of a data set naturally comes at the 
# expense of accuracy, but the trick in dimentionality reduction is to trade a little
# accuracy of simplicity! smaller data sets are easier to explore, compute and visualize
# that makes analyzing data much faster and easier without extraneous variables to process.

# In short, the idea of PCA is simple, reduce the number of variables of a data set,
# Whie preserving as much information as possible.

# 1.a.ii
# Principal component of this "high_corr_variables" are the eigenvectors of its
# covariance matrix
cov(high_corr_variables)
cor(high_corr_variables)
cor_plt(high_corr_variables)
# recall that cov calculates the similarities between the variables using dot product,
# while correlatio is the standardized dot product, in other words, a standardized
# version of covariance matrix

eigen_vectors =eigen(cov(high_corr_variables))$vectors # eigen vectors of covariance of high_corr_variables
colnames(eigen_vectors) = c('PC1', 'PC2', 'PC3', 'PC4')
row.names(eigen_vectors) = names(high_corr_variables)
knitr::kable(eigen_vectors)

eigen_values = eigen(cov(high_corr_variables))$values # eigen values of covariance of high_corr_variables
eigen_values

# confirm with principle components analysis
high_corr_var_pca = prcomp(high_corr_variables)
high_corr_var_pca

# add a table explianing eigen vectors and values

# 1.a.iii
high_corr_var_pca$center
high_corr_var_pca$sdev # square roots of the eigenvalues of the covariace matrix
# verify with the eigenvalues we calculated
sqrt(eigen_values)

# x returns the centered data multiply by the rotation matrix
Scores = high_corr_var_pca$x
knitr::kable(head(Scores))

knitr::kable(head(Scores[, 'PC1']))

ggbiplot(high_corr_var_pca, labels=rownames(high_corr_var_pca))
fviz_pca_biplot(high_corr_var_pca)
# the idea of principal component analysis is that it tries to put maximum possible information in the
# first components, then the maximum remaining information in the second and so on, until having something like shown in the scree plot below.
fit <- high_corr_variables %>% scale()
res.pca <- PCA(fit, graph=FALSE)
fviz_eig(res.pca, addlabels=TRUE)
# organizing information in pc this way will allow you to reduce dimensionality 
# without losing much informationby discarding the components with low information
# and considering the remaining components as your new variables.

# Geometrically speaking, pc represent the directions of the data that explains
# a maximal amount of variance, in other words, the lines that capture most information
# of the data.

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)


# 1.b.i
car_log$scores <- Scores[,'PC1']
# 1.b.ii
summary(
  lm(log.mpg.~
       log.acceleration.+
       model_year+
       factor(origin)+
       scores,
     data=car_log)
)
# 1.b.iii
sapply(high_corr_variables, \(x) {max(x)-min(x)})
# these four scales have different ranges
# since PCA is quite sensitive regarding the variances of the initial variables.
# variables with larger ranges will dominate over those with small ranges, which will lead to 
# biased results.
high_corr_var_pca = prcomp(high_corr_variables, scale. = TRUE)
Scores = high_corr_var_pca$x
car_log$scores <- Scores[,'PC1']

summary(
  lm(scale(log.mpg.)~
       scale(log.acceleration.)+
       scale(model_year)+
       factor(origin)+
       scores,
     data=car_log
  )
)

# variables are now transformed into same scale,
# and column scores is very significant relative to 
# the other columns.

# 2.a
questions <- readxl::read_excel('data/security_questions.xlsx',
                                        sheet=1,
                                        col_names = c('Question', 'Description'))
responds <- readxl::read_excel('data/security_questions.xlsx',
                              sheet=2,
                              col_names = TRUE)

knitr::kable(cov(responds))
sapply(responds, \(x){max(x)-min(x)})
# they are in the same scale -> no need of scaling
respond_pca <- prcomp(responds)
summary(respond_pca)
respond_pca
# 2.b
respond_eigen <- eigen(cor(responds))
knitr::kable(respond_eigen$values)

ggbiplot(respond_pca, labels=rownames(respond_pca))
fviz_pca_biplot(respond_pca)
# eigenvalue>1 and consider only the factors before the elbow on the scree plot
# according to the scree plot, the third pc does not lie before the elbow, despite the fact that it has an eigenvalue bigger than 1,
# I will choose only the first 2 pc
res.pca <- PCA(responds)
fviz_eig(res.pca, addlabels=TRUE)

# 2.c
respond_eigen$values %>% subset(respond_eigen$values>=1)
respond_pca
# the first pc seems to give equal weights to every factor,
# while the second PC gives a larger weight to Q17 Q12 and Q4.
# So, PC1 and PC2 not only capture more variance than the original data on average,
# they also offer sifnificantly more variance than the remaining PCs.

# 3.a
interactive_pca()
#Standard deviations (1, .., p=2):
#  [1] 36.96495 18.43857

#Rotation (n x k) = (2 x 2):
#  PC1       PC2
#x  0.8330720 0.5531646
#y -0.5531646 0.8330720

# 3.b
