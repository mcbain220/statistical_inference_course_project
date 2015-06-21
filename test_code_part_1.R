# R code for Statistical Inference Project

# Calculate the sample mean of an exponential distribution and compare to theoretical mean.

n <-  40
lambda <- .2
sim <- 1000

set.seed(8675309)

sample_mean <- NULL

for(i in 1:sim) sample_mean = c(sample_mean,mean(rexp(n,lambda )))

str(sample_mean)
summary(sample_mean)

theoretical_mean <- 1/lambda

# Calculate the sample variance of an exponential distrobution and compare to theoretical variance

sample_var <- var(sample_mean)
theoretical_var <- ((1/lambda)^2)/n

# graph comparison of sample distribution to normal distribution.

hist(scale(sample_mean), probability=T)  #add some labels and what not
curve(dnorm(x,0,1), -3,3, add=T)