library(manipulate)

# Plot a distribution
plotdist <- function(xseq, xdens, col, xlim, type, lty, lwd, segments=NULL, qlty, qcol, polyfill=NULL) {
  if (type == "plot") {
    plot(xseq, xdens, type="l", lwd=0, col=col, frame=FALSE, xlim=xlim, lty=lty, ylab='', xlab='')
  }
  
  if (!is.null(polyfill)) {
    polygon(polyfill[,1], polyfill[,2], col=qcol, border=qcol)
  }
  
  # draw quantile lines
  if (!is.null(segments)) {
    segments(x0=segments[,1], x1=segments[,1], y0=0, y1=segments[,2], lwd=lwd, col=qcol, lty=qlty)
  }
  
  lines(xseq, xdens, type="l", lwd=lwd, col=col, lty=lty)
}

# Plot the t distribution
plott <- function(lwd=2, ncp=0, df=300, col=rgb(0.30,0.50,0.75), xlim=c(-3,3), type="plot", lty="solid", quants=NULL, qlty="solid", qcol=rgb(0.30,0.50,0.75, 0.5), fill_quants=NULL) {
  xseq = seq(ncp-6, ncp+6, length=1000)
  xdens = dt(xseq, ncp=ncp, df=df)
  if (length(xlim) == 0) {
    xlim = c(ncp-3.5, ncp+3.5)
  }
  
  segments <- NULL
  polyfill <- NULL
  
  if (!is.null(quants)) {
    xquants = qt(quants, ncp=ncp, df=df)
    dquants = dt(xquants, ncp=ncp, df=df)
    segments = cbind(xquants, dquants)
  }

  if(!is.null(fill_quants)) {
    polyq = qt(fill_quants, ncp=ncp, df=df)
    polyfill.x = seq(polyq[1], polyq[2], by=0.001)
    polyfill.y = dt(polyfill.x, ncp=ncp, df=df)
    polyfill.x = c(polyfill.x[1], polyfill.x, tail(polyfill.x,1))
    polyfill.y = c(0, polyfill.y, 0)
    polyfill <- cbind(polyfill.x, polyfill.y)
  }
  
  plotdist(xseq, xdens, col, xlim, type, lty, lwd, segments, qlty, qcol, polyfill)
}

t_null_plot <- function(df, alpha) {
  plott(df=df, col=rgb(0.75, 0.1, 0.1), qcol=rgb(1, 0.5, 0.5), xlim=c(-6, 6), fill_quants=c(1-alpha, 0.999))
}

t_alt_lines <- function(df, ncp=0, alpha) {
  blue <- rgb(0.1, 0.1, 0.75)
  lightblue <- rgb(0.4, 0.4, 1, 0.3)
  quants <- c(0.5)
  power_quant <- pt(qt(1-alpha, df=df), df=df, ncp=ncp)
  plott(df=df, ncp=ncp, type='lines', lty="dashed", col=blue, quants=quants, qcol=lightblue, xlim=c(-6, 6), fill_quants=c(power_quant, 0.999))
}

t_test_plot <- function(diff, sd, n, alpha) {
  df=n-1
  t = diff/(sd/sqrt(n))
  t_null_plot(df, alpha)
  t_alt_lines(df,t, alpha)
}

manipulate(
  t_test_plot(diff, sd, n, alpha),
  diff  = slider(0, 4, step = 0.1, initial = 0.5),
  sd    = slider(1, 5, step = 0.1, initial = 4),
  n     = slider(2, 500, step = 1, initial = 100),
  alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)

#Q1 (a)
  #(i) this scenario creates systematic error. Sampling bias occurs when some members of a population are more likely to be included in the study than others, it reduces the generalizability of your findings because your sample is not representative of the whole population.
  #(ii) diff and sd will increase
  #(iii) By left shifting the alternative hypothesis, it will increase power to reject the null hypothesis
  #(iv) Type I error becomes more likely to happen because of this senario.

#   (b)
  #(i) this scenario creates random error.
  #(ii) n, the number of sample will decrease
  #(iii) By right shifting the alternative hypothesis, your power to reject the null hypothesis will decrease.
  #(iv) Type II error is more likely to occur.

# (c)
  #(i) this scenario has neither of the errors.
  #(ii) the value of the t-statistics' alpha will be affected.
  #(iii) Your power to reject the null hypothesis decrease because of the increase value of the critical value.
  #(iv) Type II error will be more likely to occur.

# (d)
  #(i) this scenario creates both systematic and random errors.
  #(ii) it would affect the diff, sd and n of the t-statistics. Since more data is about to be included in your sample.
  #(iii) Therefore, it increases your power to reject the null hypothesis.
  #(iv) Type I error will be more likely to occur.

#Q2
library(ggplot2)
library(ggpubr)

sderr <- function(data){
  return(sd(data)/sqrt(length(data)))
}

p_test <- function(p, sig_level){
  if(p > sig_level) cat("We do not have enough evidence to reject the Null hypothesis!")
  else cat("We have enough evidence to reject the Null hypothesis!")
}

t_test <- function(t, cutoff){
  if(t>cutoff[1] && t<cutoff[2]) cat("We do not have enough evidence to reject the Null hypothesis!")
  else cat("We have enough evidence to reject the Null hypothesis!")
}

CI <- function(data, confidence){
  v1 = (1-confidence)/2
  v2 = confidence+v1
  ci <- mean(data)+qt(c(v1, v2), length(data))*sderr(data)
  return(ci)
}

plt <- function(data, claims=claims, confidence=0.99){
  v1 = (1-confidence)/2
  v2 = confidence+v1
  ggplot()+
    aes(data)+
    geom_density()+
    geom_vline(xintercept = mean(data), color="cornflowerblue")+
    geom_vline(xintercept = quantile(data, c(v1, v2)), color="red", lty="dashed")+
    geom_vline(xintercept = claims, lwd=1.2)
}

verizon <- read.csv('verizon.csv')
time <- verizon$Time
claims = 7.6

p1 <- ggplot()+aes(time)+geom_density(color="cornflowerblue", lwd=1.2)+geom_vline(xintercept = mean(time), color="red", lty="dashed")
p2 <- ggplot()+aes(time)+geom_boxplot(color="cornflowerblue")+geom_vline(xintercept = mean(time), color="red", lty="dashed")
ggarrange(p1, p2, ncol=1, nrow=2)

# classical method

ci_99 <- CI(time, 0.99)

cat("99% CI: ", ci_99)


t <- (mean(time)-claims)/sderr(time)
p <- 1- pt(t, length(time))
cat("t: ", t, "\np: ", p)
p_test(p, 0.005)

# bootstrapping method
seed = round(runif(1)*10^9)
seed
set.seed(seed)

boot_sample <- function(data, claims){
  resample <- sample(data, length(data), replace=TRUE)
  boot_mean <- mean(resample)
  boot_mean_diff <- mean(resample)-claims
  boot_t <- (mean(resample)-claims)/sderr(resample)
  
  return(c(boot_mean, boot_mean_diff, boot_t))
}

boot_statistics <- replicate(2000, boot_sample(time, claims))

boot_mean = boot_statistics[1, ]
CI(boot_mean, 0.99)
plt(boot_mean, claims, 0.99)
boot_mean_diff = boot_statistics[2, ]
plt(boot_mean_diff, 0)
boot_t = boot_statistics[3, ]
plt(boot_t, 0)

p1 <- plt(boot_mean, claims, 0.99)+ggtitle('Boot Mean')
p2 <- plt(boot_mean_diff, 0)+ggtitle('Boot Difference')
p3 <- plt(boot_t, 0)+ggtitle('Boot t')

graphs <- ggarrange(p1, p2, p3, ncol=1, 
          font.label = c(size=10),
          common.legend = TRUE, 
          legend="bottom")
annotate_figure(graphs, top = text_grob("Bootstrapped", color="red", face="bold", size=14))

t_test_plot(diff=mean(time)-claims, sd=sd(time), n=length(time), alpha=0.005)
# as we can observe from the plot above, it visualizes the classical method we conducted,
# that the p-value is slightly larger than the significance level.

#(a)
t.test(time, mu=claims, alternative = "greater", conf.level = 0.99)

power.t.test(n=length(time), 
             delta = mean(time)-claims,
             sd = sd(time),
             alternative = "one.sided",
             sig.level = 0.001)

#(b)
t <- (mean(time)-claims)/sderr(time)
t

boot_null_alt <- function(data, claims){
  resample <- sample(data, length(data), replace=TRUE)
  null <- (mean(resample)-mean(data))/sderr(resample)
  alt <- (mean(resample)-claims)/sderr(resample)
  return(c(null, alt))
}

boot_t_stat <- replicate(10000, boot_null_alt(time, claims))

t_null <- boot_t_stat[1,]
t_alt <- boot_t_stat[2,]

cutoff <- quantile(t_null, c(0.005, 0.995))
t > cutoff[1] && t < cutoff[2]
t_test(t, cutoff)

ggplot()+
  geom_density(aes(t_alt), color="cornflowerblue", lwd=1.2)+
  geom_density(aes(t_null), lty="dashed")+
  geom_vline(xintercept=t, color="cornflowerblue")+
  geom_vline(xintercept=cutoff, lty="dotted", col="red", lwd=1)

#p
null_prob <- ecdf(t_null)
p <- 1-null_prob(t)
p_test(p, 0.01)
#power
alt_prob <- ecdf(t_alt)
alt_power <- 1-alt_prob(cutoff[2])
alt_power>0.5



install.packages('tinytex')

# or the development version on Github
remotes::install_github('yihui/tinytex')

