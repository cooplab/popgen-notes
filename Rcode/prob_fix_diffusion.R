N<-5e3
s<-seq(-0.5e-3,1e-3,length=1000)
prob.fix<-(1-exp(-s))/(1-exp(-2*N*s))

plot(s,prob.fix,type="l",lwd=3,xlab="selection coeff., s",ylab=expression(paste("Prob. of fixation, ",pi( 1/ 2*N[e]))))
points(0,1/(2*N),pch=19,cex=1.5,cex.axis=1.2,cex.lab=1.4)

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
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/prob_fix_diffusion.pdf")

my.s<-c(-1e-6,-1e-5,-1e-4,-1e-3,-1e-2)
my.s<-c(my.s,-rev(my.s[-1]))
N<-10^seq(1,7,length=100)
plot(x=range(N),c(1e-5,1e5),log="xy",ylab=expression(paste(log[10]," Ratio of Prob. of fixation ", pi( 1/ 2*N[e]), " to Neutral Prob.")),xlab=expression(paste(log[10](N[e])) ),type="n",axes=FALSE) #/(1/2*N[e])))
axis(1,at=10^(1:7),label=expression(10,10^2,10^3,10^4,10^5,10^6,10^7),cex.axis=1.2)
axis(2,at=10^(c(-(5:1),0,1:5)),label=expression(10^{-5},10^{-4},10^{-3},10^{-2},10^{-1},1,10^{1},10^{2},10^{3},10^{4},10^{5}),cex.axis=1.2,las=2) #,1:5
my.s.expression<-expression(-10^{-6},-10^{-5},-10^{-4},-10^{-3},-10^{-2},10^{-2},10^{-3},10^{-4},10^{-5})
abline(h=1,col="grey",lwd=2)
for(i in 1:length(my.s)){
	s=my.s[i]
	prob.fix<-(1-exp(-s))/(1-exp(-2*N*s))
	lines(N,prob.fix/(1/(2*N)),lwd=2)
	if(s<0){ text(N[max(which((prob.fix/(1/(2*N))) > 0.01))]/2.5,y=0.01,my.s.expression[i],cex=1.2)
	}else{
		text(N[min(which((prob.fix/(1/(2*N))) > 100))]/2.5,y=100,my.s.expression[i],cex=1.2)
	}
}
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/prob_fix_diffusion_var_N.pdf")