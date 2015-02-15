################################################################################
#### Long distance dispersal analysis
#### 8 Oct 2006. Modified 28 Jan 2015.
################################################################################
source("code/fit.disp.R")
source("code/fitModels.R")

assdist <-read.table("datasets/distances.txt",header=TRUE,sep="\t",dec=",",na.strings="NA")

str(assdist)
`data.frame:	557 obs. of  15 variables:
 $ cohort     : int  1997 1997 1997 1997 1997 1997 1997 1997 1997 1997 ...
 $ seedcode   : Factor w/ 557 levels "S_001","S_002",..: 1 2 3 4 5 6 7 9 10 11 ...
 $ endocarp   : Factor w/ 557 levels "1843_C_T1","1843_C_T2",..: 23 24 25 26 38 27 28 30 31 32 ...
 $ tree_source: Factor w/ 115 levels "1065","1069",..: 55 54 54 110 75 22 84 50 101 82 ...
 $ x1.trap    : num  175.7 130.6  75.5 270.4 200.7 ...
 $ y1.trap    : num  465 196 176 783 329 ...
 $ x2.tree    : num  132 143 143 374 233 ...
 $ y2.tree    : num   146  143  143 1016  413 ...
 $ dist       : num  321.5  55.0  75.1 254.5  89.9 ...
 $ censor     : Factor w/ 2 levels "c","e": 2 2 2 2 2 2 2 2 2 2 ...
 $ loc        : Factor w/ 3 levels "DCHA","ESQ","IN": 1 1 1 2 3 3 3 3 3 3 ...
 $ point      : Factor w/ 137 levels "","A1","A10",..: 42 78 20 126 132 43 65 67 73 88 ...
 $ code       : Factor w/ 147 levels "","DCHAA10","DCHAA3",..: 6 12 13 22 142 60 79 81 87 100 ...
 $ mhab       : Factor w/ 7 levels "A","AA","N","P",..: 1 4 4 7 7 1 3 3 3 4 ...
 $ popsource  : Factor w/ 9 levels "CL","CM","CT",..: 4 4 4 4 4 4 4 4 4 4 ...
summary(assdist$dist)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.0    13.0    62.2   574.0   273.0 16400.0 

################################################################################
x<-assdist$dist
require(MASS)
truehist(x,xlim=c(0,20000),prob=F,h=100,col=1,xlab="Distance (m)",ylab="Probability",ymax=350)
rug(x,side=1,col="red")

wpop.dist<-assdist$dist[assdist$popsource=="NC"]
summary(wpop.dist)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.0     9.6    29.9    73.7    96.0   771.0 
#------------------------------------------------------------
within<-x[x<1200]
qqnorm(within)
qqline(within)
#------------------------------------------------------------
# Smooth density estimation
# Pedro. Sevilla, 28Apr2009.
library(KernSmooth)
plot(x=c(0,1100),y=c(0,0.015),type="n",
     xlab="Distance (m)",ylab="Density")
rug(within)
for (i in 1:100) {
lines(density(sample(within,replace=TRUE)), col="grey") }
lines(density(within),col=2)
abline(v=0,lty=3)
#------------------------------------------------------------
# Kolmogorov test
A<-assdist$dist[assdist$mhab=="A"|assdist$mhab=="AA"]
P<-assdist$dist[assdist$mhab=="P"]
plot(x=c(0,1100),y=c(0,0.015),type="n",
     xlab="Distance (m)",ylab="Density")
rug(A); rug(P,col=2)
lines(density(A,bw=15),col=1)
lines(density(P,bw=15),col=2)
for (i in 1:100) {
lines(density(sample(A,replace=TRUE),bw=20), col="grey") }
for (i in 1:100) {
lines(density(sample(P,replace=TRUE),bw=20), col=2) }
#------------------------------------------------------------

truehist(wpop.dist,
	xlim=c(0,1000),
	h=10,col=1,		 # 10 m intervals
	main="Within-population dispersal",
	xlab="Distance (m)",
	ylab="Probability")
rug(wpop.dist,side=1,col="red")

truehist(wpop.dist,
	xlim=c(0,1100),ylim=c(0,110),
	h=10, # 10 m intervals
	prob=F,col=1,
	main="Within-population dispersal",
	xlab="Distance (m)",
	ylab="Number of dispersal events")
rug(wpop.dist,side=1,col="red")
mean(wpop.dist)
var(wpop.dist)

#-----------------------------
hist(wpop.dist,
	xlim=c(0,1000),
	ylim=c(0,0.05),
	breaks=100,col=1,		 # 10 m intervals
	main="Within-population dispersal",
	xlab="Distance (m)",
	freq=FALSE,
	ylab="Probability")
rug(wpop.dist,side=1,col="red")

xvec <- seq(0,1000,by=1) 
x<-wpop.dist
lines(xvec,dnorm(xvec,mean(x),sd(x)),lwd=2,col="red") 
lines(xvec,dgamma(xvec,mean(x)),lwd=2,col="bisque1") 
lines(xvec,dweibull(xvec,mean(x)),lwd=2,col="green2")
lines(xvec,dcauchy(xvec,mean(x)),lwd=2,col="yellow1")
lines(xvec,dlnorm(xvec),lwd=2,col="turquoise3")
lines(xvec,dexp(xvec),lwd=2,col="blue")
#-----------------------------
###   fun: the function assumed for the dispersal distance distribution.
###        Values allowed are: "exponential", "weibull", "gamma", "pearson",
###        "lognormal", "half-normal", "half-cauchy", "half-t", or "user"
###        (a user-defined fonction). The default is to fit an exponential
###        model.

fit.disp(wpop.dist+1,fun="exponential")
fit.disp(x+1,fun="weibull")
fit.disp(x+1,fun="gamma")
fit.disp(x+1,fun="pearson")
fit.disp(x+1,fun="lognormal")
fit.disp(x+1,fun="half-normal")
fit.disp(x+1,fun="half-cauchy")
fit.disp(x+1,fun="half-t")
funcs<-cbind("exponential", "weibull", "gamma", "pearson","lognormal", "half-normal", "half-cauchy", "half-t")

fitmodels(wpop.dist+1)   ### Long time. Fits all 8 models
    n   Model			Dev		AIC		np
1 437	exponential 	4644.5 	4646.5  1
1 1000	weibull 		5112 	5116  	2
1 1000	half-cauchy 	3759.3 	3761.3  1
1 1000	half-t 			4019.3 	4021.3  1

################################################################################
### Distributions
################################################################################


x <- rnorm(1000,rgamma(100,2,1),0.4) 
hist(x,freq=FALSE,ylim=c(0,0.45)) 
mean(x) 
var(x) 
xvec <- seq(-2,10,by=0.1) 
lines(xvec,dnorm(xvec,mean(x),sd(x)),lwd=2,col="red") 
lines(xvec,dgamma(xvec,mean(x)^2/var(x),var(x)/mean(x)),lwd=2,col="blue") 

### Another related situation is a discrete distribution that looks (e.g. Poisson), but has too many zeros to fit a Poisson distribution. You can put together a combined distribution which gives the probability of a complete failure (i.e. zero) as a single binomial trial with probability p, and then gives the size of the sample for successes as a Poisson distribution with parameter l. (The overall probability of zero will be p + (1-p) exp(-l), the probability of complete failure plus the probability of non-failure times the probability of a zero in the case of non-failure.) 
### Here is an example of this: 

rxpois <- function(n,p,lambda) { 
  # function to give compounded Poisson; 
  #  zero with probability p, otherwise Poisson(lambda) 
  v <- rpois(n,lambda)    # vector of Poisson deviates with mean lambda 
  die <- rbinom(n,1,p)    # vector of dead/alive trials 
  v[die==1] <- 0 
  v 
} 
x <- rxpois(200,0.2,4) 
plot(as.table(table(x)/length(x)),bty="l",ylab="Frequency", 
     xlim=c(0,14)) 
pvec <- seq(0,14) 
points(pvec,dpois(pvec,4)*(1-0.2),pch=16,col="red")

################################################################################

################################################################################
### fit.disp.R                    Emmanuel Paradis     24-1-2001
###
### This file contains an R function fit.disp() that fits a probabilistic model
### to dispersal data with 3 different kinds of precision (see Paradis et al.
### "Modelling large-scale dispersal distances").
###
### This program is similar to the F77 program dispers2.for except that the 95% CI
### is here computed both with the normal approximation to the likelihood function, 
### and with profile likelihood (as in dispers2.for).
###
### The followings are accepted as data input (see below for the indicator vector y):
###
###    (i) a single numeric vector (x), the observations are thus taken as exact;
###   (ii) a numeric vector (x) and an indicator vector (y) as a censoring 
###        ("e" or "c"), the observations are thus either exact or right-censored;
###  (iii) two numeric vectors (x and d2) and an indicator vector (y), the
###        observations are thus of any precision;
###   (iv) a matrix or data.frame (x) containing three vectors as in (iii).
###
###   The indicator vector (y) can be either of mode character with three values
###   allowed ("e", "i", or "c"), or of mode numeric with three different values:
###   the first one being equivalent to "e", the second one to "i", and the third
###   one to "c".
###
### The other options are:
###   fun: the function assumed for the dispersal distance distribution.
###        Values allowed are: "exponential", "weibull", "gamma", "pearson",
###        "lognormal", "half-normal", "half-cauchy", "half-t", or "user"
###        (a user-defined fonction). The default is to fit an exponential
###        model.
###   pdf: the corresponding probability density function (can be omitted
###        except if `fun = "user"').
###   CFD: the corresponding cumulative probability density function
###        (can be omitted except if `fun = "user"').
###    np: the number of parameters in the model (can be omitted except if
###        `fun = "user"').
###     p: the initial values of the parameters used by the fitting algorithm.
###        (defaults to 0.5 for all parameters).
###
### The function returns a list with the following components:
###   dev        deviance
###   aic        Akaike information criterion
###   estimate   parameter estimates
###   se         standard-errors of the parameters
###   residuals  deviance residuals
###   interval   interval lengths
