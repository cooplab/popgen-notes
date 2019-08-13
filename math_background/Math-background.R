library("RColorBrewer")

##binomial
my.cols<-brewer.pal(3,"Dark2")
my.pch<-c(19,21,22)
my.ps<-c(0.1,0.5,0.7)
layout(1:2)
par(mar=c(3,3.5,1,1))
plot(0:10,dbinom(x=0:10, size=10, prob=my.ps[1]),type="n",lwd=2,cex.axis=1.2,xlab="",ylab="")
abline(v=my.ps[1]*10,col="grey",lwd=2)
abline(v=my.ps[2]*10,col="grey",lwd=2)
abline(v=my.ps[3]*10,col="grey",lwd=2)
points(0:10,dbinom(x=0:10, size=10, prob=my.ps[1]),type="b",pch=my.pch[1],lwd=2,col=my.cols[1])


mtext("Probability",side=2,line=2.4,cex=1.4)
mtext("i",side=1,line=2,cex=1.4)
points(0:10,dbinom(x=0:10, size=10, prob=my.ps[2]),type="b",pch=my.pch[2],lwd=2,col=my.cols[2])
points(0:10,dbinom(x=0:10, size=10, prob=my.ps[3]),type="b",pch=my.pch[3],lwd=2,col=my.cols[3])


plot(0:100,dbinom(x=0:100, size=100, prob=my.ps[1]),type="n",lwd=2,cex.axis=1.2,xlab="",ylab="")
abline(v=my.ps[1]*100,col="grey",lwd=2)
abline(v=my.ps[2]*100,col="grey",lwd=2)
abline(v=my.ps[3]*100,col="grey",lwd=2)
points(0:100,dbinom(x=0:100, size=100, prob=my.ps[1]),type="b",pch=my.pch[1],lwd=2,col=my.cols[1])

mtext("Probability",side=2,line=2.4,cex=1.4)
mtext("i",side=1,line=2,cex=1.4)
points(0:100,dbinom(x=0:100, size=100, prob=my.ps[2]),type="b",pch=my.pch[2],lwd=2,col=my.cols[2])

points(0:100,dbinom(x=0:100, size=100, prob=my.ps[3]),type="b",pch=my.pch[3],lwd=2,col=my.cols[3])
legend(x="topright",col=my.col,pch=my.pch,legend(paste("p = ",my.ps,sep=""),cex=1.4)

##Geometric


par(mar=c(3,3.5,1,1))
plot(1:21,dgeom(x=0:20, prob=0.5),type="b",pch=19,lwd=2,cex.axis=1.2,xlab="",ylab="")
abline(v=1/0.5,col="grey",lwd=2)
points(1:21,dgeom(x=0:20, prob=0.1),type="b",pch=22,lwd=2)
abline(v=1/0.1,col="grey",lwd=2)
mtext("Probability",side=2,line=2.4,cex=1.4)
mtext("i",side=1,line=2,cex=1.4)
