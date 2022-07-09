plt_mean_sd <- function(data, title){
  plot(density(data), main=title)
  cat(paste("Mean: ", mean(data), "Std: ", sd(data)))
}

# a
rnorm_std <- rnorm(100000, mean=940, sd=190)
plt_mean_sd(rnorm_std, "rnorm with mean=940, sd=190")
# we expected the mean to be 940, and sd to be 190. since the normal distribution follows the central tendency ruls
# b
# ...


add_ci_segment <- function(ci95_low, ci95_high, ci99_low, ci99_high, 
                           sample_means, indices, good=TRUE) {
  segment_colors <- list(c("lightcoral", "coral3", "coral4"),
                         c("lightskyblue", "skyblue3", "skyblue4"))
  color <- segment_colors[[as.integer(good)+1]]
  
  segments(ci99_low, indices, ci99_high, indices, lwd=3, col=color[1])
  segments(ci95_low, indices, ci95_high, indices, lwd=3, col=color[2])
  points(sample_means, indices, pch=18, cex=0.6, col=color[3])
}

# Visualize the confidence intervals of samples drawn from a population
#   e.g.,
#     visualize_sample_ci(sample_size=300, distr_func=rnorm, mean=50, sd=10)
#     visualize_sample_ci(sample_size=300, distr_func=runif, min=17, max=35)
visualize_sample_ci <- function(num_samples = 100, sample_size = 100, title="density plot",
                                pop_size=10000, distr_func=rnorm, ...) {
  # Simulate a large population
  population_data <- distr_func(pop_size, ...)
  pop_mean <- mean(population_data)
  pop_sd <- sd(population_data)
  
  # Simulate samples
  samples <- replicate(num_samples, 
                       sample(population_data, sample_size, replace=FALSE))
  
  # Calculate descriptives of samples
  sample_means = apply(samples, 2, FUN=mean)
  sample_stdevs = apply(samples, 2, FUN=sd)
  sample_stderrs <- sample_stdevs/sqrt(sample_size)
  ci95_low  <- sample_means - sample_stderrs*1.96
  ci95_high <- sample_means + sample_stderrs*1.96 
  ci99_low  <- sample_means - sample_stderrs*2.58
  ci99_high <- sample_means + sample_stderrs*2.58
  
  # Visualize confidence intervals of all samples
  # Blank Corpus
  plot(NULL, xlim=c(pop_mean-(pop_sd/2), pop_mean+(pop_sd/2)), main=title, 
       ylim=c(1,num_samples), ylab="Samples", xlab="Confidence Intervals")
  
  add_ci_segment(ci95_low, ci95_high, ci99_low, ci99_high,
                 sample_means, 1:num_samples, good=TRUE)
  
  # Visualize samples with CIs that don't include population mean
  bad = which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) |
                ((ci99_low > pop_mean) | (ci99_high < pop_mean)))
  add_ci_segment(ci95_low[bad], ci95_high[bad], ci99_low[bad], ci99_high[bad],
                 sample_means[bad], bad, good=FALSE)
  
  # Draw true population mean
  abline(v=mean(population_data))
}

# a
visualize_sample_ci(distr_func=rnorm, mean=20, sd=3, title="100 samples")
# i)100*5%=5
# ii) 100*1%=1

# b
visualize_sample_ci(sample_size = 300, distr_func=rnorm, mean=20, sd=3, title="300 samples")

# m*n array
par(mfrow=c(1,2))
visualize_sample_ci(distr_func=rnorm, mean=20, sd=3, title="Normal(100 samples)")
visualize_sample_ci(sample_size = 300, distr_func=rnorm, mean=20, sd=3, title="Normal(300 samples)")
# i) for the sample size getting larger, indicates that you've gained more insight of the population
# therefore, you can guess the population mean and sd(population) more accurately, 
# which narrower their range of 95% and 99% of CI.

# c
par(mfrow=c(2,2))
visualize_sample_ci(distr_func=rnorm, mean=20, sd=3, title="Normal(100 samples)")
visualize_sample_ci(sample_size = 300, distr_func=rnorm, mean=20, sd=3, title="Normal(300 samples)")
visualize_sample_ci(distr_func=runif, title="Unif(100 samples)")
visualize_sample_ci(sample_size = 300, distr_func=runif, title="Unif(300 samples)")
# i) obviously we can observe from the graph, as the sample size gets larger, we can estimates the population more clearly.
# the confidence interval does get narrower as the sample size increases.






bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]
hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
par(mfrow=c(1,1))
plot(density(minday), main="Minute (of the day) of first ever booking", col="blue", lwd=2)

#a
sample_sd <- function(x){
  variance <- sum((x-mean(x))^2)/(length(x)-1)
  sqrt(variance)
}

property <- function(data, type="s", CI=0.95){
  if(CI==0.90) t <- c(-1.65, 1.65)
  if(CI==0.95) t <- c(-1.96, 1.96)
  if(CI==0.99) t <- c(-2.58, 2.58)
  
  Mean <- mean(data)
  if(type=="p"){
    Std <- sd(data)
  
    cat("Population")
  }else{
    Std = sqrt(sum((data-mean(data))^2)/(length(data)-1))
    Stderr <- Std/sqrt(length(data))
    ci <- (Mean+(t*Std)/sqrt(length(data)))
    
    cat("Sample", "\nStderr: ", Stderr, "\nCI range: ", ci)
  }
  cat(paste("\nMean: ",Mean , "\nStd: ", Std))
}
  # i) traditional way
# if we want to estimate a population mean, we want to calculate a CI.
# the 95% confidence interval is: xbar +- 2*sd/sqrt(n)

resample <- sample(minday, 500, replace=TRUE)
   # ii) boot strap
resamples <- replicate(2000, sample(resample, length(resample), replace=TRUE))

property(resample)
property(resamples)
property(minday)

  # iii) Visualization
plot(NULL, lwd=0,xlim=c(500, 1500), ylim=c(0, 0.0065), main="Minday vs. Bootstrapped samples")
plt_resample_mean <- function(data){
  lines(density(data), col=adjustcolor("lightblue", alpha.f = 0.05))
  return(mean(data))
}
plt_resample_median <- function(data){
  lines(density(data), col=adjustcolor("lightblue", alpha.f = 0.05))
  return(median(data))
}
resamples_mean <- apply(resamples, 2, FUN=plt_resample_mean)
  # iv) Estimation
property(resamples_mean)

lines(density(resample), lwd=3, col=adjustcolor("black", alpha.f = 0.5))
lines(density(minday), lty="dashed")

# b
par(mfrow=c(1, 2))
median(minday)
plt_resample_statistics <- function(data, resamples, method){
  
  cat(method(data))
  if(mean(data)==method(data)) {
    title<-"Means"
    transparent=0.005}
  if(median(data)==method(data)){
    title<-"Medians"
    transparent=0.2}
  
  plot(density(data), xlim=c(500, 1500), ylim=c(0, 0.0055),
       main=c("Resamples", title),cex.lab=0.75, cex.axis=0.6, cex.main=1,
       col="blue", lty="dashed")
  
  lines(density(resample), lwd=3, col=adjustcolor("black", alpha.f = 0.5))
  
  plt_resampling <- function(data){
    abline(v = method(data), col=adjustcolor("red", alpha.f = transparent))
  }
  apply(resamples, 2, FUN=plt_resampling)
  abline(v = method(resamples), lwd=2)
}
plt_resample_statistics(minday, resamples, median)
plt_resample_statistics(minday, resamples, mean)

   # iii)
resamples_median <- apply(resamples, 2, FUN=plt_resample_median)
property(resamples_median)

