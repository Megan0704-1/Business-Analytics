

seq_sample <- 1:22
resample <- sample(seq_sample, replace=TRUE)

re_sample <- c(1, rep(2,2), rep(3,3), rep(4,4), rep(5,3), rep(6,2), 7)
resample <- sample(re_sample, replace=TRUE)
plot(density(re_sample), lwd=3, prob = TRUE, ylim=c(0,0.25))
lines(density(resample), lty="dashed")

# USE THE UNSEEN POPULATION

a <- rnorm(n=100000, mean=150, sd=15)
b <- rnorm(n=200000, mean=190, sd=25)
c <- rnorm(n=500000, mean=255, sd=25)
d <- rnorm(n=200000, mean=310, sd=20)
population = c(a, b, c, d)
pop_mean <- mean(population)

plot(density(population), lty='dashed', col="blue")
abline(v=pop_mean, lty="dashed")

# TAKING ONE SAMPLE

sample_size <- 300
sample0 <- sample(population, sample_size)
sample0_mean <- mean(sample0)

lines(density(sample0), col="blue", lwd=3)
abline(v = sample0_mean, lwd=3)

# resample from our original sample

sample0
resample <- replicate(3000, sample(sample0, length(sample0), replace=TRUE))
plot(density(population), ylim=c(0, 0.009), lwd=0, main="Population vs. bootstrapped samples")

plt_resample_density <- function(sample_data){
  lines(density(sample_data), col=rgb(0.0, 0.4, 0.0, 0.01))
  return(mean(sample_data))
}
sample_means <- apply(resamples, 2, FUN = plt_resample_density)
lines(density(sample0), lwd=3)
lines(density(population), lwd=2, lty='dashed')

plot(density(population), col="blue", lty='dashed')
lines(density(sample0), col="blue", lwd=2)
plt_resample_mean <- function(sample_data){
   abline(v=mean(sample_data), col=rgb(0.0, 0.4, 0.0, 0.01))
}
apply(reamples, 2, plt_resample_mean)
abline(v = mean(sample_means), lwd=2)
abline(v = pop_mean, lty="dashed")
