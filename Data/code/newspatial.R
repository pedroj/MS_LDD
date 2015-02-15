####################################################################
# New analyses. Zahara, Mar 2010
# Pedro Jordano.
####################################################################
# Setting ppp objects for tree maps
prumap <-read.table("prumap.dat",header=TRUE,sep="\t",dec=".",na.strings=".")
attach(prumap)		# All trees, N= 258, incl. CSABINAS
names(prumap)
[1] "MICROHAB" "POINT"    "NAME"     "TREE"     "x"        "y"       
prumap.ppp <- ppp(prumap$x, prumap$y, 
                  window=owin(c(0,500),c(0,1100)),
                  unitname=c("metre","metres"))
plot(prumap.ppp,pch=16)

attach(prumap196)		# Cris trees, N=196, incl. CSABINAS
tree196.ppp <- ppp(x, y,                   
                   window=owin(c(0,500),c(0,1100)),
                   unitname=c("metre","metres")),
                   marks=prumap196$SEX)
plot(tree196.ppp)
? F H 
1 2 3 

summary(tree196.ppp)
Marked planar point pattern: 196 points
Average intensity 0.000349 points per unit area
Marks:
  frequency proportion intensity
?        32      0.163  0.000057
F        62      0.316  0.000111
H       102      0.520  0.000182

Window: rectangle = [ -10 , 500 ] x [ 0 , 1100 ]
Window area =  561000 

# ppp objects for trap data 1997-2000-JLGC
attach(traps.9700)
sd9798 <- ppp(x, y, c(0,500), c(0,1200), marks=sd9798)
sd9899 <- ppp(x, y, c(0,500), c(0,1200), marks=sd9899)
sd9900 <- ppp(x, y, c(0,500), c(0,1200), marks=sd9900)
sdl97 <- ppp(x, y, c(0,500), c(0,1200), marks=sdl97)
sdl98 <- ppp(x, y, c(0,500), c(0,1200), marks=sdl98)
sdl99 <- ppp(x, y, c(0,500), c(0,1200), marks=sdl99)
sdl <- ppp(x, y, c(0,500), c(0,1200), marks=sdl)
sd <- ppp(x, y, c(0,500), c(0,1200), marks=sd)

# density/ha
 dens= (239/(400*600))*10000
 dens
[1] 9.9583

# expected distance to nearest neighbor
 expnndist= (sqrt(dens))/2
 expnndist
[1] 1.5778

# a plot of L(t) against distance
cosa1<-Kest(prumap.ppp)
plot(cosa1, col=c(1,0,0,1), lwd=c(2,2,2,2), lty=c(1,1,1,2), main="")
plot(cosa1, sqrt(./pi)-r~r, col=c(2,1,1,3), lwd=c(2,0.2,0.2,2), lty=c(1,1,1,2), main="",ylab="L(r)")
cosa1.env=envelope(prumap.ppp,Kest)
plot(cosa1.env,sqrt(./pi)-r~r, lwd=c(2,1,1,1), lty=c(1,1,3,3), col=c(1,1,2,2), xlab="r", ylab="L(r)",main="")

L<-Lest(prumap.ppp) 
plot(L,main="Lfunction")
# Simultaneous critical envelope for L function
# (alternatively, use Lest)
plot(envelope(prumap.ppp, Kest, transform=expression(sqrt(./pi)),global=TRUE))
 
# Pair correlation
plot(pcf(prumap.ppp))
# O-ring function
lambda=prumap.ppp$n/area.owin(prumap.ppp$window)
plot(pcf(prumap.ppp), (.*lambda)~r, ylim=c(0,150)*lambda, col=c(1,0,3), lwd=c(2,0,1),main="", xlab="r (cm)", ylab="O(r)")

plot(envelope(prumap.ppp, pcf, transform=expression(sqrt(./pi)),global=TRUE),lwd=c(2,0.3,0.3,0.5), lty=c(1,1,3,3), col=c(2,1,1,1))

# Simulaciones de Monte Carlo para estima de CI
E <- envelope(prumap.ppp, Kest, nsim = 39, rank = 1) 
E
plot(E,main="Pointwise envelopes")

# the plot of Ripley's K
# setting the region
 ppregion(xl = 0, xu = 500, yl = 0, yu = 1100)
 par(mfrow=c(1,2),pty="s")
 plot(prumap.ppp,pch=16)
 plot(Kfn(prumap.ppp,50),type="s",xlab="Distance",ylab="L(t)")
 lines(lims$x,lims$lower,lty=2)
 lines(lims$x,lims$upper,lty=2)

# number of trees in 10 x 10 m squares
 attach(prumap)
 plot(x,y)
 xt<-cut(x,seq(0,500,10))
 yt<-cut(y,seq(0,1100,10))
 count<-as.vector(table(xt,yt))
 table(count)
count
   0    1    2    3    4    5 
2436  118   25   14    6    1 

# Variogram on counts of trees per plot

library(sgeostat)
pru.point<-point(pruquad, x='x', y='y')
pru.prs <- pair(pru.point,15)
pru.v<-est.variogram(pru.point,pru.prs,'z')
plot(pru.v)

# Delaunay triangulations
attach(prumap)
pru.tr<-tri.mesh(x,y)

plot(pru.tr)

par(mfrow=c(1,2))	# draw map and triangulation side by side
plot(x,y, pch=16)
plot(pru.tr,col="red", pch=16)

summary.tri(pru.tr)

triangulation:
Call: tri.mesh(x = x, y = y) 
number of nodes: 239 
number of arcs: 703 
number of boundary nodes: 11 
number of triangles: 465 
number of constraints: 0 

print.tri(pru.tr)

# Number of trees in 10 x 10 m squares
 attach(prumap)
 plot(x,y,pch=16)
 xt<-cut(x,seq(0,500,10))
 yt<-cut(y,seq(0,1100,10))
 count<-as.vector(table(xt,yt))
 table(count)
 count
   0    1    2    3    4 
5320  132   26   14    8 

# Quadratcounts
par(mfrow=c(1,2))
Q<-quadratcount(prumap.ppp, nx = 25, ny = 55) # Quadrats 20 x 20 m
plot(prumap.ppp, cex = 0.5, pch = "+") 
plot(Q, cex = 0.5)

contour(density(prumap.ppp,10),axes=FALSE)
plot(density(prumap.ppp,10)) 
plot(prumap.ppp,add=TRUE, cex = 0.5, pch = "+")

M <- quadrat.test(prumap.ppp, nx = 25, ny = 55) # Quadrats 20 x 20 m
M 
plot(Q) 
plot(M, add = TRUE, cex = 0.3)

# KS Test
KS <- kstest(prumap.ppp, function(x, y) {x}) 
plot(KS) 

