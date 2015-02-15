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


for (i in 1:258) lines(c(x[i],x[nn[i]]),c(y[i],y[nn[i]]))
