library("RColorBrewer")
library("latex2exp")



library(VennDiagram)

grid.newpage()
draw.triple.venn(area1 = 22, area2 = 18, area3 = 13, n12 = 10, n23 = 0, n13 = 0, 
    n123 = 0, category = c("Dog", "Cat", "Lizard"), lty = "blank", 
    fill = c( "pink1","skyblue", "mediumorchid"),cex=1.4,cat.cex=1.4,lwd=3)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/dist_pics/Venn_toys.pdf")
    


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
legend(x="topright",col=my.cols,pch=my.pch,legend=paste("p = ",my.ps,sep=""),cex=1.4)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/dist_pics/Binomial.pdf")
##Geometric


par(mar=c(3,3.5,1,1))
my.cols<-brewer.pal(3,"Set1")
my.pch<-c(19,21,22)
my.ps<-c(0.9,0.5,0.1)
plot(1:16,dgeom(x=0:15, prob=my.ps[1]),type="n",cex.axis=1.2,xlab="",ylab="")
abline(v=1/my.ps[1],col="grey",lwd=2)
abline(v=1/my.ps[2],col="grey",lwd=2)
abline(v=1/my.ps[3],col="grey",lwd=2)
points(1:21,dgeom(x=0:20, prob=my.ps[1]),type="b",pch=my.pch[1],col=my.cols[1],lwd=2)
points(1:21,dgeom(x=0:20, prob=my.ps[2]),type="b",pch=my.pch[2],col=my.cols[2],lwd=2)
points(1:21,dgeom(x=0:20, prob=my.ps[3]),type="b",pch=my.pch[3],lwd=2,,col=my.cols[3])
legend(x="topright",col=my.cols,pch=my.pch,legend=paste("p = ",my.ps,sep=""),cex=1.4)

mtext("Probability",side=2,line=2.4,cex=1.4)
mtext("i",side=1,line=2,cex=1.4)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/dist_pics/Geometric.pdf")


par(mar=c(3,3.5,1,1))
my.cols<-brewer.pal(3,"Set1")
my.pch<-c(19,21,22)
my.lambdas<-c(1,5,10)
plot(0:15,dpois(x=0:15, lambda=my.lambdas[1]),type="n",cex.axis=1.2,xlab="",ylab="")
abline(v=my.lambdas[1],col="grey",lwd=2)
abline(v=my.lambdas[2],col="grey",lwd=2)
abline(v=my.lambdas[3],col="grey",lwd=2)

points(0:100,dbinom(x=0:100, size=100, prob=1/100),type="b",pch=my.pch[2],lwd=2,col=adjustcolor(my.cols[1],0.5))
points(0:100,dbinom(x=0:100, size=100, prob=5/100),type="b",pch=my.pch[2],lwd=2,col=adjustcolor(my.cols[2],0.5))
points(0:100,dbinom(x=0:100, size=100, prob=10/100),type="b",pch=my.pch[2],lwd=2,col=adjustcolor(my.cols[3],0.5))

points(0:15,dpois(x=0:15, lambda=my.lambdas[1]),type="b",pch=my.pch[1],col=my.cols[1],lwd=2)
points(0:15,dpois(x=0:15, lambda=my.lambdas[2]),type="b",pch=my.pch[2],col=my.cols[2],lwd=2)
points(0:15,dpois(x=0:15, lambda=my.lambdas[3]),type="b",pch=my.pch[3],lwd=2,col=my.cols[3])
#legend(x="topright",col=my.cols,pch=my.pch,legend=paste(TeX("$\\lambda = $"),my.lambdas,sep=""),cex=1.4)
legend(x="topright",col=my.cols,pch=my.pch,legend=c(TeX("$\\lambda = 1$"),TeX("$\\lambda = 5$"),TeX("$\\lambda = 10$")),cex=1.4)

mtext("Probability",side=2,line=2.4,cex=1.4)
mtext("i",side=1,line=2,cex=1.4)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/dist_pics/Poisson.pdf")



library("MASS")
layout(t(1:3))
par(mar=c(4,4,0.5,0.1))
X<-mvrnorm(n=40,mu=c(5,2),Sigma=matrix(c(1,0.0,0.0,1),byrow=TRUE,nrow=2))
plot(X,pch=19,cex=1.5,xlab="",ylab="",cex.axis=1.2)
mtext("Y",side=2,line=2.5,cex=1.4)
mtext("X",side=1,line=2.5,cex=1.4)
legend(x="topleft",legend=signif(cor(X)[1,2],2),cex=1.4)

X<-mvrnorm(n=40,mu=c(5,2),Sigma=matrix(c(1,0.8,0.8,1),byrow=TRUE,nrow=2))
print(cor(X))
plot(X,pch=19,cex=1.5,xlab="",ylab="",cex.axis=1.2)
mtext("X",side=1,line=2.5,cex=1.4)
legend(x="topleft",legend=signif(cor(X)[1,2],2),cex=1.4)

X<-mvrnorm(n=40,mu=c(5,2),Sigma=matrix(c(1,-0.6,-0.6,1),byrow=TRUE,nrow=2))
print(cov(X))
plot(X,pch=19,cex=1.5,xlab="",ylab="",cex.axis=1.2)
mtext("X",side=1,line=2.5,cex=1.4)
legend(x="topright",legend=signif(cor(X)[1,2],2),cex=1.4)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/dist_pics/Covar.pdf")



X<-mvrnorm(n=20,mu=c(5,2),Sigma=matrix(c(1,0.5,0.5,1),byrow=TRUE,nrow=2))
print(cor(X))
plot(X,pch=19,cex=1.5,xlab="",ylab="",cex.axis=1.2)
mtext("X",side=1,line=2.5,cex=1.4)
mtext("Y",side=2,line=2.5,cex=1.4)

lm.model<-lm(X[,2]~X[,1])
abline(lm.model,lwd=2)
sapply(1:nrow(X),function(i){
lines(rep(X[i,1],2), c(X[i,2],X[i,2]-lm.model$resid[i]),col="red",lwd=2,lty=3)
}
)
abline(lm.model,lwd=2)
points(X,pch=19,cex=1.5)
my.nums<-signif(c(var(X[,1]),cov(X[,1],X[,2]), lm.model$coeff[2]),2)
legend(x="bottomright",legend= paste(c("Var(X)=","Cov(X,Y)=","Slope="),my.nums),cex=1.4)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/dist_pics/Linear_regression.pdf")

