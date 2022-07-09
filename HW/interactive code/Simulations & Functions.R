customers = read.table("customers.txt", header=TRUE)
ages = customers$age
hist(ages)
hist(ages, breaks=50, col="lightblue")
# breakpoints between hist cells
hist(ages, breaks=seq(0, 100, 10))
h <- hist(ages)
h$breaks
h$counts
h$density

#Visualizations
plot(density(ages), main="Age Distribution")
hist(ages, breaks=50, prob=TRUE)
lines(density(ages, adjust=0.5), col="blue", lwd=2)

#Dispersion
summary(ages)
Q1 = quantile(ages, 1/4)
Q3 = quantile(ages, 3/4)
iqr = IQR(ages) #unname(Q3-Q1)

#outliers
ages[ages<Q1-1.5*iqr | ages>Q3+1.5*iqr]

visual <- boxplot(ages, horizontal = TRUE)
stripchart(ages, method="stack", add=T)
visual$out

# Statistics

#1. Deviation
ages-mean(ages)

#2. Absolute deviation
abs(ages-mean(ages))

# Mean Absolute Deviation
sum(abs(ages-mean(ages)))/length(ages)
mean(abs(ages-mean(ages)))

# Median Absolute Deviation
median(abs(ages-mean(ages)))

#3. Variability
sum((ages-mean(ages))^2)

#4. Variance
sum((ages-mean(ages))^2)/(length(ages)-1)

#5. Standard Deviation
sqrt(sum((ages-mean(ages))^2)/(length(ages)-1))
