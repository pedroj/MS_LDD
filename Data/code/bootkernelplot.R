#------------------------------------------------------
# Plot randomized kernel with specific mean and SD values.

# Dispersal Kernel
dd<-abs(rnorm(1000, mean = 20, sd = 415))

# We can omit the truehist and step directly to the Kernel.
truehist(dd, xlim=c(0,max(dd)),
         # ylim=c(0,0.012),
         prob=T,h=10,xlab="Distance (m)",
         ylab="Probability",col=rgb(0, 0, 1, 0.2),
         lty=0)
rug(x,side=1,col="red")

# Kernel
d<-density(kern$dist,bw=20,from=0,to=max(dd)) # add density estimate
lines(d,xlim=c(0,max(dd)),col="blue")

# These are resampled (randomized) values to visualize a confidence envelope.
for (k in 1: 100) {
	di<- sample(dd, length(dd), replace = T, prob = NULL)
	ddi<-density(di, bw=20, from=0, to=max(dd)) # add density estimate
	lines(ddi,xlim=c(0, max(dd)), col="grey", lwd=0.2)

# TDKs
# Model species-specific kernels with different means and variances.

# Dispersal Kernel
dd1<- abs(rnorm(1000, mean = 5, sd = 10))
dd2<- abs(rnorm(1000, mean = 20, sd = 40))
dd3<- abs(rnorm(1000, mean = 20, sd = 120))
dd4<- abs(rnorm(1000, mean = 40, sd = 445))

totd<- c(dd1,dd2,dd3,dd4)

# Kernels
# Total kernel (histogram and rug)
truehist(totd, xlim=c(0,max(totd)),
         # ylim=c(0,0.012),
         prob=T,h=10,xlab="Distance (m)",
         ylab="Probability",col=rgb(0, 0, 1, 0.2),
         lty=0)
rug(x,side=1,col="red")

# Species-specific kernels
d1<-density(dd1,bw=20,from=0,to=max(dd1)) # add density estimate
lines(d1,xlim=c(0,max(dd1)),col="blue")
d2<-density(dd2,bw=20,from=0,to=max(dd2)) # add density estimate
lines(d2,xlim=c(0,max(dd2)),col="orange")
d3<-density(dd3,bw=20,from=0,to=max(dd3)) # add density estimate
lines(d3,xlim=c(0,max(dd3)),col="green")
d4<-density(dd4,bw=20,from=0,to=max(dd4)) # add density estimate
lines(d4,xlim=c(0,max(dd4)),col="black")








