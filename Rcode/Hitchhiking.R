fig.direct="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/Figs/"


s=c(0.01,0.001)
ne=1000
r=1E-8

tau<-function(s,ne){2*log(2*ne)/s}
physical.pos<-seq(-150e3,150e3,length=2000)
rec.dist<-abs(physical.pos)*r
plot(physical.pos,(1-exp(-rec.dist*tau(s[1],ne))),ylim=c(0,1),typ="l",lwd=2,xlab="Physical Position",ylab=expression(pi/theta),cex.axis=1.5,cex.lab=1.3)
lines(physical.pos,(1-exp(-rec.dist*tau(s[2],ne))),lty=2,lwd=2,col="red")
legend("bottomright",legend=paste("s = ",s),col=c("black","red"),lty=c(1,2),lwd=1.5,cex=1.5)
dev.copy2eps(file=paste(fig.direct,"hitchhiking_reduction.eps",sep=""))
