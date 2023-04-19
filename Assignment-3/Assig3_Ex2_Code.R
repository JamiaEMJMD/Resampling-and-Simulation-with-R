km    <- c(37388,44758,45833,30862,31705,34010)
price <- c(14636,14122,14016,15590,15568,14718)
df <- data.frame(km, price) # y is price, x is km
n  <- length(price)
fit<-lm(df[,2]~df[,1])
s<-summary(fit)

# Obtaining intercept and slope directly from data
theta<-c(s$coefficients[1], s$coefficients[2])
intercept <-theta[1]
slope <- theta[2]

# Standard deviation of each one of the components
sdtheta<-c(s$coefficients[3], s$coefficients[4])

# Price estimation
fitb <- lm(price~km, data=df)
pred <- predict(fitb, data.frame(km = c(50000)), se.fit=TRUE)
initp <- pred$fit        # Initial price prediction
sd_initp <- pred$se.fit  # Standard deviation for initial price prediction

# Parameters for simulation

nb <- 1000 
y <- seq(1:n) # 
pb <- numeric(nb) # Create a vector to store predictions


############## Quantile/percentile bootstrap method

# Quantile bootstrap to estimate the price for the car with 50000km

for(i in 1:nb){
  yb <- sample(y, n, replace=T) # Sample 1000 times values index from 1 to 6
  dfb = data.frame(km = df[yb, 1], price = df[yb, 2])
  fitb <- lm(price~km, data = dfb)
  p <- predict(fitb, data.frame(km = c(50000)))
  pb[i] <- p
}

hist(pb)

mean(pb) # Bootstrap estimate of the price

ic <- quantile(pb,c(0.025,0.975)) # 95% confidence interval:
ic

############## Simple method

# The simple method to calculate 95% Confidence Interval:

lb <- 2*initp - quantile (pb,0.975)
ub <- 2*initp - quantile (pb,0.025)
ic <- c(lb,ub)
names(ic) <- c("2.5%", "97.5%")
ic


############## Bootstrap-t method

set.seed(123)

pb <- list()
sdb <- list()

for (i in 1:nb){
  yb <- sample(y,n,replace =T)
  df_yb = data.frame(price = df[yb,2], km = df[yb,1])
  fitb <- lm(price~km, data=df_yb)
  price <- predict(fitb, data.frame(km = c(50000)), se.fit=TRUE)
  pb[[i]] <- price$fit
  sdb[[i]] <- price$se.fit
  
}

pb <- data.frame(price = unlist(pb))
sdb <- data.frame(sd = unlist(sdb))

# Estimating t value for each sample

tb_price <- (pb$price - initp)/sdb
tb <- data.frame(tb_price)
names(tb) <- c("price")

tb_pl <- quantile(tb$price,0.025) # Lower CI for tb 
tb_pu <- quantile(tb$price,0.975) # Upper CI for tb

pl <- initp + tb_pl * sdp # Lower CI for price
pu <- initp + tb_pu * sdp # Upper CI for price

ic <- c(pl,pu)
ic

