n <- c(13,40,9,20,18) # Number of individuals in each species

# Create function to calculate Simpson's diversity index (D)

SimpsonIndex <- function(n) {
  N <- sum(n)
  D <- 1-sum(n*(n-1)/(N*(N-1))) # D = 1-(∑n(n-1))/(N(N-1))
  return(D)
}
SimpsonIndex(n)

# Bootstrap estimation of D
l <- length(n)

# Set number of bootstrap samples
nsim <- 1000
stat <- numeric(nsim ) #create a vector in which to store the results

# Set up a loop to generate a series of bootstrap samples

for (i in 1:nsim){
     nB= sample (n , l, replace =T)
     stat[i] = SimpsonIndex(nB) # Calculate D for all bootstrap samples
     }

hist(stat)

# Bootstrap estimate of D
DB<- mean(stat)
DB

# Estimated standard error: 
SE<-sd(stat)
SE

# 95% Confidence Interval: 
quantile(stat,c(0.025,0.975))

# The simple method to calculate 95% Confidence Interval:
2*SimpsonIndex(n) - quantile (stat,0.975)
2*SimpsonIndex(n) - quantile (stat,0.025)


