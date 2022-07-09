system.time(read.csv('piccollage_accounts_bundles.csv'))
#slow

library(data.table)
system.time(fread('piccollage_accounts_bundles.csv'))
# much faster

ac_bundle_dt <- fread('piccollage_accounts_bundles.csv')
ac_bundle_matrix <- as.matrix(ac_bundle_dt[, -1])

# SPARSE DATA
library(lsa)
system.time(cosine(ac_bundle_matrix))
# very slow

library(qlcMatrix)
system.time(qlcMatrix::cosSparse(ac_bundle_matrix))
# much faster

## Choosing "Sweet Mother's Day"
terms <- "mom|mother|dad|father"
bundle_names <- colnames(ac_bundle_matrix)
grep(terms, bundle_names, ignore.case = TRUE, value=TRUE)

# COSINE RECOMMENDATION
cosine_recommendation <- function(m){
  cos_m <- qlcMatrix::cosSparse(m)
  diag(cos_m)=2
  sort_rows <- function(m){
    names(sort(m, decreasing = TRUE))
  }
  sorted_m <- t(apply(cos_m, 2, sort_rows))
  sorted_m <- sorted_m[, -1]
  
  return(sorted_m[,1:5])
}
cosine_rec <- cosine_recommendation(ac_bundle_matrix)
cosine_rec['sweetmothersday',]

#CORRELATION
# create a mean of bundles first
bundle_mean <- apply(ac_bundle_matrix, 2, mean)
bundle_mean_m <- t(replicate(nrow(ac_bundle_matrix), bundle_mean))
correlation_rec <- ac_bundle_matrix-bundle_mean_m
corr_rec <- cosine_recommendation(correlation_rec)
corr_rec['sweetmothersday',]

#ADJUSTED-COSISE recomendation
account_mean <- apply(ac_bundle_matrix, 1, mean)
account_mean_m <- replicate(ncol(ac_bundle_matrix), bundle_col_mean)
ad_cosine_recommendation <- ac_bundle_matrix-account_mean_m
account_rec <- cosine_recommendation(ad_cosine_recommendation)
account_rec['sweetmothersday',]
