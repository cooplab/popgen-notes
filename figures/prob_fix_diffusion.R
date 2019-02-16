N<-5e3
s<-seq(-0.5e-3,1e-3,length=1000)
prob.fix<-(1-exp(-s))/(1-exp(-2*N*s))

plot(s,prob.fix,type="l",lwd=3,xlab="selection coeff., s",ylab=expression(paste("Prob. of fixation, ",pi( 1/ 2*N[e]))))
points(0,1/(2*N),pch=19,cex=1.5)

N<-2e3
prob.fix<-(1-exp(-s))/(1-exp(-2*N*s))

lines(s,prob.fix,type="l",lwd=3,col="red")
points(0,1/(2*N),pch=19,cex=1.5,col="red")
N<-10e3
prob.fix<-(1-exp(-s))/(1-exp(-2*N*s))

lines(s,prob.fix,type="l",lwd=3,col="blue")
points(0,1/(2*N),pch=19,cex=1.5,col="blue")
lines(s[s>0],s[s>0],lty=3,lwd=3)
legend(x="topleft", legend=c(expression(N[e] == 2000),expression(N[e] == 5000),expression(N[e] == 10000),expression(N[e]==infinity ~p[F]==s)),col=c("red","black","blue","black"),lwd=3,lty=c(rep(1,3),3))
dev.copy2pdf(file="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/figures/prob_fix_diffusion.pdf")


