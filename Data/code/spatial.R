5-15 Feb 2004

 ls()
 [1] "dist"         "last.warning" "mmhab"        "pm.pp"        "prugeo"      
 [6] "prumap"       "prumap.pp"    "sd9798"       "sd9899"       "sd9900"      
[11] "sdl97"        "sdl98"        "sdl99"        "tt"          

--- 
dev.copy2eps("distplot&map.eps",width=6,height=6)
dev.print(device=pdf,"plotout2.pdf")
---

#----------
# distance between two points
dist<-function(x1, y1, x2, y2)  sqrt((x2 - x1)^2 + (y2 - y1)^2)

#  to estimate nearest neighbor for n points and draw lines
# after plotting. Substitue 100 by no. of points 

r<-numeric(258)
nn<-numeric(258)
d<-numeric(258)
for (i in 1: 258) {
d<-0
for (k in 1: 258) d[k]<-dist(x[i],y[i],x[k],y[k])
r[i]<-min(d[-i])
nn[i]<-which(d==min(d[-i]))						
}
# plot the locations of seed traps and the lines joining to each source tree
for (i in 1: 258) lines(c(x[i],x[nn[i]]),c(y[i],y[nn[i]]))   # draw the lines
#-------------

# Setting the ppp objects for seeds and seedlings
attach(traps.9700)
R > names(traps.9700)
 [1] "SITE"       "MHAB"       "POINT"      "LABEL"      "OX"        
 [6] "OY"         "x"          "y"          "AREA"       "SITE2"     
[11] "MICRO"      "MICROCAMPO" "sdl97"      "sd9798"     "sdl98"     
[16] "sd9899"     "sdl99"      "sd9900"     "sd"         "sdl"       
plot(x,y)
sd9798 <- ppp(x, y, c(0,500), c(0,1200), marks=sd9798)
sd9899 <- ppp(x, y, c(0,500), c(0,1200), marks=sd9899)
sd9900 <- ppp(x, y, c(0,500), c(0,1200), marks=sd9900)
sdl97 <- ppp(x, y, c(0,500), c(0,1200), marks=sdl97)
sdl98 <- ppp(x, y, c(0,500), c(0,1200), marks=sdl98)
sdl99 <- ppp(x, y, c(0,500), c(0,1200), marks=sdl99)
sdl <- ppp(x, y, c(0,500), c(0,1200), marks=sdl)
sd <- ppp(x, y, c(0,500), c(0,1200), marks=sd)
par(mfrow=c(1,1))
plot(sd9798)
ls()
 [1] "last.warning" "pru.nb"       "pru.point"    "pru.prs"      "pru.tr"      
 [6] "pru.v"        "prumap"       "prumap196"    "pruquad"      "sd"          
[11] "sd9798"       "sd9899"       "sd9900"       "sdl"          "sdl97"       
[16] "sdl98"        "sdl99"        "traps.9700"  
R > 

# Setting ppp objects for tree maps
prumap <-read.table("prumap.dat",header=TRUE,sep="\t",dec=".",na.strings=".")
attach(prumap)		# All trees, N= 258, incl. CSABINAS
plot(x,y)
names(prumap)
[1] "MICROHAB" "POINT"    "NAME"     "TREE"     "x"        "y"       
prumap.ppp <- ppp(x, y, c(0,500), c(0,1100))
plot(prumap.ppp,pch=16)

attach(prumap196)		# Cris trees, N=196, incl. CSABINAS
tree196.ppp <- ppp(x, y, c(0,500), c(0,1100), marks=SEX)
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
R>


#	prumap: coordinates x y z, with z=1 for all datapoints
 attach(prumap)
 plot(x,y,pch=16)
 dist<-function(x1, y1, x2, y2)  sqrt((x2 - x1)^2 + (y2 - y1)^2)

 r<-numeric(258) 
 nn<-numeric(258)
 d<-numeric(258)
 for (i in 1:258) {
 d<-0
 for (k in 1:258) d[k]<-dist(x[i],y[i],x[k],y[k])
 r[i]<-min(d[-i])
 nn[i]<-which(d==min(d[-i]))
 }

# adds lines joining nearest neighbors
 for (i in 1:258) lines(c(x[i],x[nn[i]]),c(y[i],y[nn[i]]))

# density/ha
 dens= (239/(400*600))*10000
 dens
[1] 9.9583

# expected distance to nearest neighbor
 expnndist= (sqrt(dens))/2
 expnndist
[1] 1.5778

# setting the region
 ppregion(xl = 0, xu = 500, yl = 0, yu = 1100)

# a plot of L(t) against distance
 plot(Kfn(prumap.ppp,5),type="s",xlab="Distance",ylab="L(t)")
# setting the envelope for K(t)
 lims<-Kenvl(10,100,Psim(258))

# the plot of Ripley's K
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

# number of trees in 10 x 10 m squares
 attach(prumap)
 plot(x,y,pch=16)
 xt<-cut(x,seq(0,500,10))
 yt<-cut(y,seq(0,1100,10))
 count<-as.vector(table(xt,yt))
 table(count)
 count
   0    1    2    3    4 
5320  132   26   14    8 
R>
#	prumap: coordinates x y z, with z=1 for all datapoints
 attach(prumap)
 plot(x,y,pch=16)
 dist<-function(x1, y1, x2, y2)  sqrt((x2 - x1)^2 + (y2 - y1)^2)

 r<-numeric(258) 
 nn<-numeric(258)
 d<-numeric(258)
 for (i in 1:258) {
 d<-0
 for (k in 1:258) d[k]<-dist(x[i],y[i],x[k],y[k])
 r[i]<-min(d[-i])
 nn[i]<-which(d==min(d[-i]))
 }

# adds lines joining nearest neighbors
 for (i in 1:258) lines(c(x[i],x[nn[i]]),c(y[i],y[nn[i]]))

# Ripley K for inhomogeneous patterns
ls()
 [1] "last.warning" "pru.nb"       "pru.point"    "pru.prs"      "pru.tr"      
 [6] "pru.v"        "pru258.J"     "prumap"       "prumap.ppp"   "prumap196"   
[11] "pruquad"      "sd"           "sd9798"       "sd9899"       "sd9900"      
[16] "sdl"          "sdl97"        "sdl98"        "sdl99"        "traps.9700"  
[21] "tree196.ppp" 

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
 
 fit<-mpl(tree196.ppp,~polynom(x,y,2),Poisson())
 lambda<-predict(fit,newdata=data.frame(x=tree196.ppp$x,tree196.ppp$y),type="trend")
 Ki<-Kinhom(tree196.ppp,lambda)
 conspire(Ki, cbind(K, theo) ~ r, subset="r <= 300")


---
# setting the region
 ppregion(xl = 0, xu = 500, yl = 0, yu = 1100)

# a plot of L(t) against distance
 plot(Kfn(tree196.ppp,5),type="s",xlab="Distance",ylab="L(t)")
# setting the envelope for K(t)
 lims<-Kenvl(10,100,Psim(196))

# the plot of Ripley's K
 par(mfrow=c(1,2),pty="s")
 plot(tree196.ppp,pch=16)
 plot(Kfn(prumap.ppp,10),type="s",xlab="Distance",ylab="L(t)")
 lines(lims$x,lims$lower,lty=2)
 lines(lims$x,lims$upper,lty=2)

# Histograms for distance
   names(assdist)
 [1] "cohort"       "seedcode"     "endocarp"     "tree.source"  "xtrap"       
 [6] "ytrap"        "xtree"        "ytree"        "dist"         "loc"         
[11] "point"        "code"         "mhab"         "popsource"    "species.mhab"
[16] "treedens"     "dnn"          "state"       

   par(mfrow=c(3,1))
   hist(d,main="Distance between traps and trees NCH, d")
   hist(assdist$dist,main="Distance between traps and trees NCH, AssDist")
   hist(dist.mat,main="Distance between traps and trees NCH, AssDist")
hist(assdist$dist,col="black",main="Distance between traps and trees NCH, AssDist")
   truehist(d,nbins="Scott",h=10,xlim=c(0,1200),col="black",
            border="white",main="Dispersal distance, NCH seedtraps only, d")
--- 
R> dev.copy2eps("distplot&map.eps",width=6,height=6)
quartz 
     2 
Warning message: 
unknown postscript font family, using Helvetica 
R> ?dev.copy2eps
R> ?device

dev.print(device=pdf,"plotout2.pdf")
