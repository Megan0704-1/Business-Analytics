# 

#1
library(ggpubr)
library(ggplot2)
library(plot3D)
library(rgl)
library(factoextra)
library(FactoMineR)
library(magrittr)
library(psych)
path = 'data/security_questions.xlsx'

questions <- readxl::read_excel(path, sheet=1, col_names = c("Index", "Questions"))
data <- readxl::read_excel(path, sheet=2)
data_pca <- prcomp(data, scale.=TRUE)
data_pca |> sapply(head)

data_pca$rotation = -data_pca$rotation
fviz_pca_var(data_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE   
)
data_pca <- prcomp(data, scale.=TRUE)

# simulating the noise and taking its eigenvalues
sim_noise <- function(n, p){
  noise <- data.frame(replicate(p, rnorm(n))) # 33*10
  correlation <- cor(noise) # 10*10
  eigen(correlation)$values # 10, how much variance can be explained from the noise.
}
eigen_noise <- replicate(100, sim_noise(33, 10)) #100*10
eigen_noise_mean <- apply(eigen_noise, 1, mean)

res.pca <- PCA(data)
p1 <- fviz_eig(res.pca, 
               choice = 'eigenvalue', 
               addlabels=TRUE, 
               main='Original Screeplot of data (variance)')
p1 <- p1+geom_hline(yintercept = 1,color="red", lwd=1.2)


p2 <- fviz_eig(res.pca, 
               choice = 'variance', 
               addlabels=TRUE, 
               main='Original Screeplot of data (eigenvalue)')
p2 <- p2+geom_hline(yintercept = 1,color="red", lwd=1.2)

color_list <- "coral2"
p3 <- fviz_eig(res.pca, 
               choice = 'eigenvalue', 
               addlabels=TRUE, 
               main='Screeplot of data with noise (variance)')+
  geom_point(aes(x=1:10, y=eigen_noise_mean),color=color_list)+
  geom_line(aes(x=1:10, y=eigen_noise_mean),color=color_list)+
  theme_bw()
p3 <- p3+geom_hline(yintercept = 1,color="red", lwd=1.2)

ggarrange(p1, p2, ncol=2, nrow=1)
ggarrange(p1, p3, ncol=2, nrow=1)

# b.
# after conducting horn's parallel analysis, we can easily observe from the plot
# that only the first PC has a higher eigenvalues than the average "noise.
# As a result, comparing variance extracted from the original data versus noise,
# we can conclude that only a single dimension, the first PC, will be retained in our model.

# reproduce data from various pc dimensions
reproduce_data <- function(original, num_pc){
  pca_results <- prcomp(original, scale=TRUE)
  scores <- pca_results$x[, 1:num_pc]
  weights <- pca_results$rotation[, 1:num_pc]
  reproduction <- scores %*% t(weights)
  return(reproduction)
}

residual_plt <- function(original, reproduction, ndim){
  residual <- as.data.frame(original-reproduction)
  ggplot()+
    aes(x=1:nrow(data_scale), y=residual$Q1)+
    geom_point(alpha=0.7, size=2)+
    ylim(-2,2)+
    geom_hline(yintercept = 0, 
               col='salmon',
               lwd=1.2,
               lty=2)+
    ggtitle(paste0("Dimension Reduction: ", ndim))
}

data_scale <- scale(data)
ndim=3
temp = reproduce_data(data_scale, ndim)
p1 <- residual_plt(data_scale, temp,ndim=ndim)
ndim=9
temp = reproduce_data(data_scale, ndim)
p2 <- residual_plt(data_scale, temp,ndim=ndim)
ndim=12
temp = reproduce_data(data_scale, ndim)
p3 <- residual_plt(data_scale, temp,ndim=ndim)
ndim=18
temp = reproduce_data(data_scale, ndim)
p4 <- residual_plt(data_scale, temp,ndim=ndim)

ggarrange(p1, p2, p3, p4, ncol=1, nrow=4)

#2
# in simple terms, the main diff between composite model
# and factor model is that FM tends to examine a model
# fit and you interpret whether or not factors in a model
# are related, fitted or not.
# Whereas in composite models are specified such that they consist of a set of
# interrelated composites, all of which emerge as linear combinations of
# observable variables.
# analysis is for when you have a more complicated,
# either hidden or not yet found variables.

# also, FM requires larger samples, CM can be done with very small samples

# principal is simply doing a PCA for n principal components of either a
# correlation or covariance matrix. 
# Different from "princomp", "principal" returns a subset of just the best
# n factors, and the eigenvectors are rescaled by sqrt of the eigenvlaues to 
# produce the component loadings mode typical in factor analysis.
data_principal <- principal(data, nfactor=ncol(data), rotate="none", scores=TRUE)
data_principal |> sapply(head)

# ss_loadings = eigenvalues
ss_loadings <- sum(data_principal$loadings[,"PC1"]^2)
ss_loadings
data_principal$values[1]

# a.
head(data_principal$loadings[,1:3])
data_principal$Vaccounted

for (i in 1:18){
  cat(paste0('Best belong for Question ', i),': ')
  cat(names(sort(data_principal$loadings[i,], decreasing=TRUE))[1])
  cat('\n')
}

find_unique <- function(data_principal, n){
  for (i in 1:n){
    if (max(data_principal$loadings[,i]) > 0.7){
      important_ones <- data_principal$loadings[,i][data_principal$loadings[,i]>=0.7]
      important_ones <- sort(important_ones, decreasing=TRUE)
      cat("Component", i, "has good loadings:", names(important_ones))
    }else{
      important_ones <- sort(data_principal$loadings[,i], decreasing = TRUE)[1]
      cat("Component", i, "has good no loadings.\n")
      cat('The largest lambda value is: ', important_ones)
    }
    name <- names(important_ones)
    cat("\nBest fit:", name)
    cat('\n')
    cat('\n')
  }
}

find_unique(data_principal, 18)

# Items are either best belong to PC1 or PC2


#b.
data_principal$Vaccounted['Cumulative Var','PC3']

# c.
# communality = h^2
data_pca3 <- principal(data, nfactor=3, rotate='none', scores=TRUE)

community <- data_pca3$communality
community
# variances of variables explained by a specified amount of principal components.

data_pca3$uniquenesses
# 1-communality, unexplained variance of a certain variable.

# think we're dealing with an item with low communality and high uniqueness value
temp = sort(data_pca3$communality)
names(temp[temp<=0.7])
temp = sort(data_pca3$uniquenesses, decreasing = TRUE)
names(temp[temp>=0.3])

# Q2 is less adequately explained by the first 3 PC.

#d.
data_principal <- principal(data, nfactor=ncol(data), rotate='none', scores=TRUE)

#round(cor(data_principal$scores),2)

x<- unname(data_pca3$loadings[,1])
y<- unname(data_pca3$loadings[,2])
z<- unname(data_pca3$loadings[,3])

scatter3D(x, y, z,
          phi=0, 
          bty='g',
          main="Dimension Comparison",
          xlab='PC1', ylab='PC2', zlab='PC3',
          cex=2, pch=20, 
          ticktype="simple")
text3D(x, y, z, labels=paste0("Q", 1:18), add=TRUE, colkey = FALSE, cex=0.7)


x<- unname(data_pca3$scores[,1])
y<- unname(data_pca3$scores[,2])
z<- unname(data_pca3$scores[,3])

scatter3D(x, y, z,
          phi=0, 
          bty='g',
          main="Subjects scores",
          xlab='PC1', ylab='PC2', zlab='PC3',
          cex=1.2, pch=20, 
          ticktype="simple",
          type="h")

#????????????????????
#e.
find_unique(data_principal, 1)
best_fit_PC1 <- c(1:18)[-17][-12][-4]
questions[best_fit_PC1,]
# consumers seem to perceive the security of e-commerce
# sites largely on the websites' functionality, and security.

# Q3
#a.
data_principal <- principal(data, nfactor=3, rotate='none', scores=TRUE)
data_r_principal <- principal(data, nfactor=3, rotate='varimax', scores=TRUE)

data_principal$Vaccounted["Proportion Var",]
data_r_principal$Vaccounted["Proportion Var",]
# diff, and the rotated components are less then the PC

#b.
data_principal$Vaccounted["Cumulative Var",]
data_r_principal$Vaccounted["Cumulative Var",]
# they give the same cumulative cariance eventually,

#c.
best_fit_PC1
apply(data_principal$loadings[,1:3], 1, sum)
apply(data_r_principal$loadings[,1:3], 1, sum)

# looking at the table, almost every measurement items 
# increased, instead of Q4, Q12 and Q17, which are the only 3 items
# that are not best belong to PC1 originally.

# d.
for (i in 1:18){
  cat(paste0('Best belong for Question ', i),': ')
  cat(names(sort(data_r_principal$loadings[i,], decreasing=TRUE))[1])
  cat('\n')
}

find_unique(data_r_principal, 3)
find_unique(data_principal, 3)

# yes.

#e,

# RC1: concerns of the sites security (unautorized access)
# RC2: concerns of transaction transmission 
# RC3: concerns of transaction evidential protection