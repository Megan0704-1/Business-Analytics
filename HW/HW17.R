library(dplyr)

# loading data and remove missing values
cars <- read.table("data/auto-data.txt", header=FALSE, na.strings="?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", 
                 "model_year", "origin", "car_name")
cars$car_name <- NULL
cars <- na.omit(cars)

# Shuffle the rows of cars
set.seed(42)
cars <- cars[sample(1:nrow(cars)),]

# Create a log transform data set also
cars_log <- with(cars, data.frame(log(mpg),
                                  log(cylinders),
                                  log(displacement),
                                  log(horsepower),
                                  log(weight), 
                                  log(acceleration),
                                  model_year, 
                                  origin))
# Linear model of mpg over all the variables that don't have multicollinearity
cars_lm <- lm(mpg ~ weight+
                acceleration+
                model_year+
                factor(origin),
              data=cars)

# Linear model of log mpg over all the log variables, including multicollinearity
cars_log_lm <- lm(log.mpg. ~ log.weight.+
                    log.acceleration.+
                    model_year+
                    factor(origin),
                  data = cars_log)
# Linear model of log mpg over all the log variables, including multicollinear terms
cars_log_all_lm <- lm(log.mpg. ~ log.cylinders.+
                        log.displacement.+
                        log.horsepower.+ 
                        log.weight.+
                        log.acceleration.+
                        model_year+
                        factor(origin),
                       data=cars_log)

# Q1
# retrain
set.seed(42)
train_indicies <- sample(1:nrow(cars), size=0.7*nrow(cars))
train_set <- cars_log[train_indicies,]

paste0("The size of the training set is ", round(nrow(train_set)/nrow(cars)*100, 2), "%")

lm_trained <- lm(log.mpg. ~ log.weight. +
                   log.acceleration. +
                   model_year + 
                   factor(origin),  
                 data=train_set)
lm_trained$coefficients
cars_log_lm$coefficients

# Predict
test_set = cars_log[-train_indicies,]
paste0("The size of the testing set is ", round(nrow(test_set)/nrow(cars)*100, 2), "%")

log.mpg.predicted <- predict(lm_trained, test_set)
knitr::kable(head(log.mpg.predicted))

# in sample
mpg_fitted <- fitted(lm_trained) # y hat
fit_error <- train_set$log.mpg. - mpg_fitted # residuals(lm_trained)
mse_is <- mean(fit_error^2)
mse_is

# out sample
mpg_actual <- test_set$log.mpg.
pred_error <- mpg_actual-log.mpg.predicted
mse_oos <- mean(pred_error^2)
mse_oos

# show a data frame of the test set's actual log.mpg.
test_set %>%
  transmute(actual_mpg = mpg_actual,
         predicted_values = log.mpg.predicted,
         pred_error = pred_error) -> comparison_df
knitr::kable(comparison_df %>% head)


# Q2
MSE_is <- function(model, data, log=FALSE){
  if(log==FALSE){
    fit_error <- model$fitted.values-data[,1]
  }else{
    fit_error <- exp(model$fitted.values)-exp(data[,1])
  }
  insample_mse <- mean(fit_error^2)
  return(insample_mse)
}

MSE_is(cars_lm, cars)
MSE_is(cars_log_lm, cars_log, log=TRUE)
MSE_is(cars_log_all_lm, cars_log, log=TRUE)
# cars_log_all_lm has the best MSE
# cars_lm has the worst

#b,

# calculates prediction error for fold i out of k
fold_i_pred_err <- function(i, k, dataset, predictors){
  folds <- cut(1:nrow(dataset), k, labels=FALSE)
  
  test_indices <- which(folds==i)
  test_set <- dataset[test_indices, ]
  train_set <- dataset[-test_indices, ]
  trained_model <- update(predictors, data=train_set)
  
  predictions <- predict(trained_model, test_set)
  test_set[,1]-predictions
}


# calculates mse_oos across all folds
k_fold_mse <- function(predictors, data, k=10){
  shuffled_indices = sample(1:nrow(data))
  data = data[shuffled_indices,]
  
  fold_pred_error <- sapply(1:k, \(i){
    fold_i_pred_err(i, k, data, predictors)
  })
  pred_error <- unlist(fold_pred_error)
  mse <- \(errs){mean(errs^2)}
  c(in_sample = mse(residuals(predictors)), out_of_sample = mse(pred_error))
}


k_fold_mse(cars_lm,
           data=cars,
           k=10)

k_fold_mse(cars_log_lm,
           data = cars_log,
           k=10)

k_fold_mse(cars_log_all_lm,
           data=cars_log)

k_fold_mse(cars_log_lm,
           dataset = cars_log,
           k=392)

