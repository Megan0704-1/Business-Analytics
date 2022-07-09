#d1 <- rnorm(n=500, mean=15, sd=5)
#d2 <- rnorm(n=200, mean=30, sd=5)
#d3 <- rnorm(n=100, mean=45, sd=5)
#d123 <- c(d1, d2, d3)
#plot(density(d123), col="blue", lwd=2, main="Distribution 1")
#abline(v=mean(d123))
#abline(v=mean(d123), lty="dashed")

#     Q1
# negatively skewed = mean<median<mode
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

d1 <- rnorm(n=500, mean=100, sd=5)
d2 <- rnorm(n=200, mean=80, sd=10)
d3 <- rnorm(n=100, mean=60, sd=15)
d123 <- c(d1, d2, d3)
plot(density(d123), col="blue", lwd=2, main="Distribution 2")
abline(v=mean(d123))
abline(v=median(d123), lty="dashed")
abline(v=Mode(d123), lty="dotted")

datasets <- rnorm(n=800)
plot(density(datasets), main="Distribution 3", lwd=2, col="blue")
abline(v=mean(datasets), lwd=0.01, col="red")
abline(v=median(datasets), lwd=0.01, col="green")

#     Q2
# (a)
rdata <- rnorm(n=2000, mean=0, sd=1)
plot(density(rdata), col="blue")
abline(v=mean(rdata), lty="solid")
abline(v=c(sd(rdata), -sd(rdata)), lty="dashed")
abline(v=c(sd(rdata)*2, -sd(rdata)*2), lty="dashed")
abline(v=c(sd(rdata)*3, -sd(rdata)*3), lty="dashed")
# (b)
summary(rdata)
Q1 <- quantile(rdata, 1/4)
Q2 <- quantile(rdata, 2/4)
Q3 <- quantile(rdata, 3/4)
Q4 <- quantile(rdata, 4/4)
iqr <- IQR(rdata)
Q1/sd(rdata)
Q2/sd(rdata)
Q3/sd(rdata)
Q4/sd(rdata)
# (c)
new_data <- rnorm(n=2000, mean=35, sd=3.5)
plot(density(new_data))
new_mean <- mean(new_data)
abline(v=mean(new_data), lty="solid")
abline(v=c(new_mean-sd(new_data), new_mean+sd(new_data)), lty="dashed")
abline(v=c(new_mean-sd(new_data)*2, new_mean+sd(new_data)*2), lty="dashed")
abline(v=c(new_mean-sd(new_data)*3, new_mean+sd(new_data)*3), lty="dashed")
summary(new_data)
(quantile(new_data, 1/4)-mean(new_data))/sd(new_data)
(quantile(new_data, 3/4)-mean(new_data))/sd(new_data)
# (d)
plot(density(d123), xlim=c(20, 140))
abline(v=mean(d123), lty="solid")
new_mean <- mean(d123)
abline(v=c(new_mean-sd(d123), new_mean+sd(d123)), lty="dashed")
abline(v=c(new_mean-sd(d123)*2, new_mean+sd(d123)*2), lty="dashed")
abline(v=c(new_mean-sd(d123)*3, new_mean+sd(d123)*3), lty="dashed")
summary(d123)
(quantile(new_data, 1/4)-mean(new_data))/sd(new_data)
(quantile(new_data, 3/4)-mean(new_data))/sd(new_data)

#Q3
#(a)
# He suggest to use Freedman-Diaconis rule for bin widths and numbers.
# The Wikipedia article says that the FD method is less sensitive than the standard deviation to outliers in data.

#(b)
rand_data <- rnorm(n=800, mean=20, sd=5)
#bin width: h, num of bins: k   STRUGES' Formula
k <- ceiling(log(length(rand_data), base = 2))+1
h <- ceiling((max(rand_data)-min(rand_data))/k)

#bin width: h, num of bins: k   SCOTT'S normal reference rule
sample_sd <- function(x){
  variance <- sum((x-mean(x))^2)/(length(x)-1)
  sqrt(variance)
}
h <- (3.49*sample_sd(rand_data))/(length(rand_data)^(1/3))
k <- ceiling((max(rand_data)-min(rand_data))/h)

#bin width: h, num of bins: k    FD' choice
h <- 2*IQR(rand_data)/(length(rand_data)^(1/3))
k <- ceiling((max(rand_data)-min(rand_data))/h)

#(c)
out_data <- c(rand_data, runif(10, min=40, max=60))
#bin width: h; num of bins: k   STRUGES' Formula
k <- ceiling(log(length(out_data), base = 2))+1
h <- ceiling((max(out_data)-min(out_data))/k)

#bin width: h; num of bins: k   SCOTT'S normal reference rule
sample_sd <- function(x){
  variance <- sum((x-mean(x))^2)/(length(x)-1)
  sqrt(variance)
}
h <- (3.49*sample_sd(out_data))/(length(out_data)^(1/3))
k <- ceiling((max(out_data)-min(out_data))/h)

#bin width: h; num of bins: k    FD' choice
h <- 2*IQR(out_data)/(length(out_data)^(1/3))
k <- ceiling((max(out_data)-min(out_data))/h)

# The fd choice changes the least when outliers are added
FD <- function(x){
  h <- 2*IQR(x)/(length(x)^(1/3))
  k <- ceiling((max(x)-min(x))/h)
  c(h, k)
}
SCOTT <- function(x){
  h <- 3.49*sample_sd(x)/(length(x)^(1/3))
  k <- ceiling((max(x)-min(x))/h)
  c(h, k)
}
STRUGS <- function(x){
  k <- ceiling(log(length(x), base=2))+1
  h <- ceiling((max(x)-min(x))/k)
  c(h, k)
}

