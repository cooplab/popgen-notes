layout(t(1:3))
par(mar=c(4,4,0.1,0.1))
library("mvtnorm")
traits<-rmvnorm(50,sigma=matrix(c(1,0.6,0.6,1),nrow=2,byrow=TRUE))
num.kids<-ceiling(traits[,2])
num.kids<-num.kids-min(num.kids)
plot(traits[,1],num.kids,xlab="",ylab="")
mtext("Male antler size",side=1,cex=1.2,line=2); mtext("Number of offspring",side=2,cex=1.2,line=2)

traits<-rmvnorm(50,sigma=matrix(c(1,-0.6,-0.6,1),nrow=2,byrow=TRUE))
num.kids<-ceiling(traits[,2])
num.kids<-num.kids-min(num.kids)

plot(traits[,1],num.kids,xlab="",ylab="")
mtext("Female leg length",side=1,cex=1.2,line=2); mtext("Number of offspring",side=2,cex=1.2,line=2)
plot(rmvnorm(50,sigma=matrix(c(1,0.6,0.6,1),nrow=2,byrow=TRUE)),xlab="",ylab="")
mtext("1/2-sister's leg length",side=1,cex=1.2,line=2); mtext("1/2-brother's antler size",side=2,cex=1.2,line=2)

dev.copy2pdf(file="~/Dropbox/Courses/EVE100/EVE100_2015/Exam Questions/Red_deer_selection.pdf")