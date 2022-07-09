library(data.table)
bundle_dt <- fread('piccollage_accounts_bundles.csv')
bundle_m <- as.matrix(bundle_dt[,-1])
# Q1
# a.
# i.
## ans. 6
colnames(bundle_dt)
set.seed(125)
sticker_num = round(runif(1, 0, 166), 0)# => cute valentine
names(bundle_dt[,137])
# there's no cute valentine in piccollage, but cute valentines does
# Intuition: I'm guessing "Valentine stickers[119],
#                          "happy[44]",
#                          "saint valentine[6]",
#                          "HeartStickerPack[18]",
#                          "Valentine2013StickerPack[66]"

# ii.
names(bundle_dt[,c(6, 18, 44, 66, 119)])

# b.
# i.
#1. cosine similarity
library(lsa)
cosine_m <- cosine(bundle_m)


sort_name <- function(m){
  
  # 1 is for checking on the values
  total_m <- list()
  
  for(i in 1:length(colnames(m))){
    temp = as.matrix(t(sort(m[i,], decreasing = TRUE)))
    if(i==1){
      total_m$new_m <- colnames(temp)
      total_m$new_v <- temp
    }else{
    total_m$new_m <- rbind(total_m$new_m, colnames(temp))
    total_m$new_v <- rbind(total_m$new_v, temp)
    }
  }
  return(total_m)
}


new_m <- sort_name(cosine_m)$new_m


new_v <- sort_name(cosine_m)$new_v
row.names(new_m) <- colnames(cosine_m)
colnames(new_m) <- 1:length(colnames(cosine_m))


#2. putting it all together

recommend <- function(m, FUN='cos'){
  
  M <- list()
  
  if(FUN=='cos'){
    c_m <- cosine(m)
  }else if(FUN=='cor'){
    c_m <- cor(m)
  }
  M$new_m <- sort_name(c_m)$new_m
  M$corr_v <- c_m
  row.names(M$new_m) <- colnames(cosine_m)
  colnames(M$new_m) <- 1:length(colnames(cosine_m))
  row.names(M$corr_v) <- colnames(cosine_m)
  colnames(M$corr_v) <- colnames(cosine_m)
  return(M)
}

cosine_similarity_m <- recommend(bundle_m)$new_m
#3.
View(cosine_similarity_m)

head(cosine_similarity_m['cutevalentine',])

#ii.
#1.
cal_corr <- function(m){
  MAX <- max(ncol(m), nrow(m))
  bundle_means <- apply(m, 2, mean)
  bundle_means_m <- t(replicate(nrow(m), bundle_means))
  bundle_corr_m <- m-bundle_means_m
  if (ncol(bundle_corr_m)==MAX) {new_m <- cosine(t(bundle_corr_m))}
  else {new_m <- cosine(bundle_corr_m)}
  return(new_m)
}

bundle_cor_m <- cal_corr(bundle_m)
correlation_m <- recommend(bundle_cor_m)$new_m
correlation_v <- recommend(bundle_cor_m)$corr_v
# Just to make sure.
# correlation_m <- recommend(bundle_m, FUN='cor')
head(correlation_m['cutevalentine',])

# visualization

# Using 3 methods to get the same results.
library(corrplot)
par(mfrow=c(3,1))
corr_m <- cor(bundle_m)
corrplot(corr_m[135:145, 135:145], method='color')
corrplot(bundle_cor_m[135:145,135:145], method='color')
corrplot(correlation_v[135:145, 135:145], method='color')

#iii. adjusted cosine similarity
#Adjusted cosine similarity
#This similarity measurement is a modified form of vector-based similarity where we take into the fact that different users have different ratings schemes; in other words, some users might rate items highly in general, and others might give items lower ratings as a preference. To remove this drawback from vector-based similarity, we subtract average ratings for each user from each user's rating for the pair of items in question:

# data transformation !!!!!!trouble!!!!!
temp <- as.data.frame(bundle_dt[,-1])

temp <- transpose(temp, keep.names = 'rn')
row.names(temp) <- temp$rn
temp = as.matrix(temp[,-1])
View(temp)
adjust_cosine_m <- cal_corr(temp)

ad_cosine_m <- recommend(adjust_cosine_m)$new_m
ad_cosine_v <- recommend(adjust_cosine_m)$new_v
head(ad_cosine_m['cutevalentine',])


#c.
#d.

#2.
if(!exists('foo',mode='function')) source("demo_simple_regression.R")
interactive_regression()
#a,
#i., ii.
#raw slope of x and y:0
#correlation:0 (no relationship between them at all)

#b.
#i., ii.
# raw slope of x and y:0
# correlation:0

#c.
#i., ii.
#raw slope of x and y: positive, close to 1
# correlation: approximates to 1

#d.
#i., ii.
#rawslope od x and y: -1
#correlation:-1

#e.
# the curve shape

#f.
#two dots that are very close

#g.
#i.
pts <- interactive_regression()
#ii.
summary(lm(pts$y~pts$x))
#iii.
# estimate the correlation to be 0
cor(pts)

#iv.
standardize <- function(v){
  diff <- v - mean(v)
  std <- sd(v)
  return(diff/std)
}

standardized_df <- data.frame(x=standardize(pts$x),
                              y=standardize(pts$y))
View(standardized_df)
# I think the regression slope won't change

#v.
summary(lm(standardized_df$y ~ standardized_df$x))
cor(standardized_df)
# the values do not change at all

