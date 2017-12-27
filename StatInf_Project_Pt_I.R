####################################################
# Statistical Inference Project Part I
####################################################
####################################################

#load libraries
library(dplyr)
library(ggplot2)


## Part 1 Simulation
#
# In this project you will investigate the exponential distribution in R and compare it with the Central Limit Theorem. 
# The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. 
# The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda. Set lambda = 0.2 for all of the simulations. 
# You will investigate the distribution of averages of 40 exponentials. Note that you will need to do a thousand simulations.
# Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials. You should
# 
# Show the sample mean and compare it to the theoretical mean of the distribution.
# Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
# Show that the distribution is approximately normal.
# In point 3, focus on the difference between the distribution of a large collection of random exponentials and the distribution of a large collection of averages of 40 exponentials.


#Define Variables for simulation
set.seed(100) # set the seed value for reproducibility

nexp <- 40           #number of exponentials
lambda <- 0.2     #value for exp ditribution
nsim <- 1000      #number of simulation
quantile <- qnorm(.975) # 95th % quantile to be used in Confidence Interval 
                 # ~1.696

#create matrix for simulation
simMatrix <- matrix(rexp(nexp * nsim, rate = lambda), nsim)

#create vector with means
simMeansVec <- rowMeans(simMatrix)

#theoretical mean
theoryMean <- 1/lambda

#calculate sample mean
sampleMean <- mean(simMeansVec)

##Mean Comparison

cat("Theory Mean:",theoryMean,"Sample Mean:",sampleMean)


#plot data with comparison betweem Means
hist(simMeansVec,xlab = "Exponential Means",
     main="Histogram for Distribution Means")
abline(v=theoryMean,lty=4,lwd=4,col="blue")
abline(v=sampleMean,lty=3,lwd=4,col="red")

##Variance Comparison

sampleVariance <- var(simMeansVec)
theoryVariance <- (1 / lambda)^2 / nexp

cat("Theory Variance:",theoryVariance,"Sample Variance:",sampleVariance)

##Show distribution is approximately normal 

simMeansDFrame <- data.frame(simMeansVec)
plotdata <- ggplot(simMeansDFrame, aes(x = simMeansVec))
plotdata <- plotdata + geom_histogram(aes(y=..density..),colour="black",fill = "lightgreen",binwidth = 0.2)
plotdata <- plotdata + stat_function(fun = dnorm, args = list(mean = theoryMean, sd = sqrt(theoryVariance)), aes(colour = "Theo Var"), lwd = 1.5)
plotdata <- plotdata + stat_function(fun = dnorm, args = list(mean = sampleMean, sd = sqrt(sampleVariance)), aes(colour = "Sample Var"), lwd = 1.5, lty = 5)
pltodata <- plotdata + scale_colour_manual(values = c("blue"="blue","red"="red") )
plotdata <- plotdata + labs(title = "Density of means of 40 Samples", x = "Mean of 40 Samples", y = "Density",colour="Variance")
plotdata


##Confidence Interval Comparison
theoCI <- theoryMean + c(-1,1)*quantile*sd(simMeansVec)/sqrt(nexp)
sampleCI <- round(mean(simMeansVec) + c(-1,1)*quantile*sd(simMeansVec)/sqrt(nexp),3)
cat("Theory Conf. Int.:",theoCI,"Sample Conf. Int.:",sampleCI)