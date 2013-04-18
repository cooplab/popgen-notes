fig.direct="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/Figs/"


sel=c(0.01,0.001)

tau = 2*log(2*10000)/sel[1]
physical.pos<-seq(-150e3,150e3,length=2000)
rec.dist<-abs(physical.pos)*1e-8
plot(physical.pos,(1-exp(-rec.dist*tau)),ylim=c(0,1),typ="l",lwd=2,xlab="Physical Position",ylab=expression(pi/theta),cex.axis=1.5,cex.lab=1.3)
tau = 2*log(2*10000)/sel[2]
rec.dist<-abs(physical.pos)*1e-8
lines(physical.pos,(1-exp(-rec.dist*tau)),lty=2,lwd=2,col="red")
legend("bottomright",legend=paste("s = ",sel),col=c("black","red"),lty=c(1,2),lwd=1.5,cex=1.5)
dev.copy2eps(file=paste(fig.direct,"hitchhiking_reduction.eps",sep=""))