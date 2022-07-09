n <- 5000
flips <-2*rbinom(n, 1, 0.5)-1
means <- c(replicate(n+1, NA))
for (i in 1:n){
  means[i] <- mean(flips[1:i])
}
plot(means, type="l", ylim=c(-1, 1))
abline(h=0, col="red")
# As we expected, we see that the average num of points quickly converges to 0.

# Instead of average probabilities, what about total points?
n <- 5000
flips <- 2*rbinom(n, 1, 0.5)-1
total <- c(0)
for (i in 1:n){
  total[i] <- sum(flips[1:i])
}
plot(total, type="l",ylim=c(-300, 300))
abline(h=0, col="red")
# rather than converging, it walks away!

# How far from their starting points do random walks go on average?
# ie. if many people played the 500-flips game we've described, would they find their total points walking axay in the same manner?

# Random binomial walk function
random_binom_walk <- function(steps){
  flips <- 2*rbinom(steps, 1, 0.5)-1
  total <- c(0)
  for (i in 2:steps){
    total[i] <- total[i-1]+flips[i]
  }
  return(total)
}
# simulate 6 walks
walks <- data.frame(matrix(0, nrow=5000, ncol=6))
for (i in 1:6){
  walks[i] <- random_binom_walk(5000)
}
# Plot!
par(mfrow=c(2, 3))
for (i in 1:6){
  plot(walks[[i]], type="l", ylim=c(-200, 200))
  abline(h=0, col="red")
}

# Even with a small num of random walks, they can still go anywhere

m <- 3000
n <- 2000
walks <- data.frame(matrix(0, nrow=n, ncol=m))
for(i in 1:m){
  walks[i] = random_binom_walk(n)
}
final.points <- c(0)
for (i in 1:m){
  final.points[i] <- walks[n, i]
}
par(mfrow=c(1, 1))
hist(final.points, breaks=20, prob=TRUE)
lines(density(final.points), lwd=3, col="blue")

