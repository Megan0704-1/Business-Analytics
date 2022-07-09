library(dplyr)
library(rpart)
library(ggplot2)
library(tidyverse)
library(rpart.plot)
library(DataExplorer)

insurance <- read.csv('data/insurance.csv')

as.matrix(lapply(insurance, \(x){sum(is.na(x))}))

plot_missing(insurance)

rmse_oos <- function(groud_truth, preds){
  sqrt(mean((groud_truth-preds)^2))
}

# Q1
# a. Ordinary Least Square regression
insurance %>% glimpse
insurance_lm <- lm(charges~age+
                     sex+
                     bmi+
                     children+
                     smoker+
                     region,
                   data = insurance)
summary(insurance_lm) # age and BMI seems highly correlated to charges
plot(insurance_lm)

# b. 
insurance_tree <- rpart(charges~age+
                          sex+
                          bmi+
                          children+
                          smoker+
                          region,
                        data = insurance)
# i.
rpart.plot(insurance_tree)
# ii.
# depth=3
# iii.
# 4 leaf groups
insurance_tree$frame$var
# iv.
insurance_tree %>% summary
# 5398.85, 12299.89, 21369.22, 441692.81
# v.
# smoker==no
# age<43, bmi<30

# Q2
fold_i_pred_err <- function(i, k, dataset, model){
  # cut the data set
  folds <- cut(1:nrow(dataset), k, labels=FALSE)
  
  # pick data that is labeled as "i"
  test_indices <- which(folds==i)
  
  test_set <- dataset[test_indices, ]
  train_set <- dataset[-test_indices, ]
  
  # trained model
  trained_model <- update(model, data=train_set)
  
  predictions <- predict(trained_model, test_set)
  test_set[,length(test_set)]-predictions
}


# calculates mse_oos across all folds
k_fold_rmse <- function(model, data, k=10){
  
  # randomly shuffle the data
  shuffled_indices = sample(1:nrow(data))
  data = data[shuffled_indices,]
  
  # get prediction errors of each folds
  fold_pred_error <- sapply(1:k, \(i){
    fold_i_pred_err(i, k, data, model)
  })
  
  pred_error <- unlist(fold_pred_error)
  rmse <- \(errs){sqrt(mean(errs^2))}
  c(in_sample = rmse(residuals(model)), out_of_sample = rmse(pred_error))
}

# a.
k_fold_rmse(insurance_lm, insurance, k=nrow(insurance))
# b.
k_fold_rmse(insurance_tree, insurance, k=nrow(insurance))

# Q3.
train_indicies = sample(1:nrow(insurance),
                        size=0.8*nrow(insurance))
train_set = insurance[train_indicies,]
test_set = insurance[-train_indicies,]


bagged_train <- function(model, dataset, b=100){
  lapply(1:b, \(i){
    # get a bootstrapped data set
    train_set <- dataset[sample(1:nrow(dataset), nrow(dataset), replace=TRUE),]
    train_models = update(model, data=train_set)
  })
}

bagged_predict <- function(bagged_model, new_data){
  predictions <- lapply(1:length(bagged_model), \(i){
    predict(bagged_model[[i]], new_data)
  })
  
  # take the mean of 100 predictions on rows of mpg
  as.data.frame(predictions) |> apply(1, FUN=mean)
}

# b. regression prediction err
old_learning <- update(insurance_lm, data=train_set) |>
  predict(object=_, test_set) |>
  rmse_oos(test_set$charges, preds=_)

print(old_learning)

# bagged
bagged_train(insurance_lm, train_set, b=100) |>
  bagged_predict(test_set) |>
  rmse_oos(test_set$charges, preds=_)

print(old_learning)

boots <- seq(100, 1200, by=100)
learning <- sapply(boots, \(b){
  bagged_train(insurance_lm, train_set, b=b) |>
    bagged_predict(test_set) |>
    rmse_oos(test_set$charges, preds=_)
})

plot(boots, learning, type="l")
abline(h=old_learning, lty='dashed')

# c.
old_learning <- 
  predict(insurance_tree, test_set) |>
  rmse_oos(test_set$charges, preds=_)

print(old_learning)

bagged_train(insurance_tree, train_set, b=100) |>
  bagged_predict(test_set) |>
  rmse_oos(test_set$charges, preds=_)

boots <- seq(100, 1200, by=100)
learning <- sapply(boots, \(b){
  bagged_train(insurance_tree, train_set, b=b) |>
    bagged_predict(test_set) |>
    rmse_oos(test_set$charges, preds=_)
})

plot(boots, learning, type="l")
abline(h=old_learning, lty='dashed')


# Q3

boost_train <- function(model, dataset, target, n=100, lr=0.1){
  
  # get target column index
  target_index = which(colnames(dataset)==target)
  
  # get data.frame of only predictor variable
  predictors <- dataset[, -target_index]
  
  # Initialize residuals and models
  res <- dataset[, target_index] # get vector of GT to start

  models <- list()
  
  for(i in 1:n){
    
    # fit predictor and residuals
    new_model <- update(model, data = cbind(charges=res, predictors))
    
    # update residuals with lr: e = e - a * y_hat
    res <- res-sapply(predict(new_model, dataset), \(i){lr*i})
    
    models[[i]] <- new_model
  }
  
  list(models=models, lr=lr)
}

boost_predict <- function(boosted_training, new_data){
  boosted_model = boosted_training$models
  boosted_lr = boosted_training$lr
  
  # predict target for models and store the predictions
  predictions = lapply(boosted_model, \(model){predict(model, new_data)})
  
  predictions_frame = as.data.frame(predictions) |> unname()
  
  apply(predictions_frame, 1, sum)*boosted_lr
}

boost_train(insurance_lm, train_set, target="charges", n=1000) |>
  boost_predict(test_set) |>
  rmse_oos(test_set$charges, preds = _)

boost_train(insurance_tree, train_set, target="charges", n=1000) |>
  boost_predict(test_set) |>
  rmse_oos(test_set$charges, preds = _)


# Q4
prev_err=100000000
pred_err=10000000
err_list <- c()
depth=1
while(pred_err<prev_err){
  
  insurance_tree <- rpart(charges~age+
                            sex+
                            bmi+
                            children+
                            smoker+
                            region,
                          data = insurance,
                          maxdepth=depth)
  
  if(pred_err < prev_err){prev_err = pred_err}
  
  pred_err = bagged_train(insurance_tree, insurance) |>
    bagged_predict(insurance) |>
    rmse_oos(insurance$charges, preds = _)
  
  err_list <- c(err_list, pred_err)
  depth = depth+1
}

length(err_list)
plot(1:(depth-1), err_list, type="l")

# b.

prev_err=100000000
pred_err=10000000
err_list <- c()
depth=1
while(pred_err<prev_err){
  
  insurance_tree <- rpart(charges~age+
                            sex+
                            bmi+
                            children+
                            smoker+
                            region,
                          data = insurance,
                          maxdepth=depth)
  
  if(pred_err < prev_err){prev_err = pred_err}
  
  pred_err = boost_train(insurance_tree, insurance, target="charges") |>
    boost_predict(insurance) |>
    rmse_oos(insurance$charges, preds = _)
  
  err_list <- c(err_list, pred_err)
  depth = depth+1
}

length(err_list)
plot(1:(depth-1), err_list, type="l")
