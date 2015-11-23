#library("symbols")

layout(matrix(1:4,nrow=2,byrow=TRUE))
par(mar=c(2,2,1,1))
a<-c(0.5,1,1.5)  ##additive
plot.lm.genos(a=a,p=0.1)
plot.lm.genos(a,p=0.9)
a<-c(0.5,1,1)  ##dominant
plot.lm.genos(a,p=0.1)
plot.lm.genos(a,p=0.9)
dev.copy2pdf(file="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/figures/additive_effect.pdf")

####BALANCING SELECTION
layout(t(1:3))
par(mar=c(2,2,1,1))

a<-c(0.5,1,0.75)
p.eq<-(1-a[1])/sum(1-a[c(1,3)])

plot.lm.genos(a=a,p=0.1)
plot.lm.genos(a,p=p.eq)
plot.lm.genos(a=q,p=0.9)
dev.copy2pdf(file="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/figures/additive_effect_OverDom.pdf")



plot.lm.genos<-function(a,p){
	HWE<-c((1-p)^2,2*p*(1-p),p^2)
	#symbols(x=0:2,y=a-sum(a*HWE),circles=HWE,ylim=range(a-sum(a*HWE)))
	plot(y=range(a)*c(.2,1.8),x=c(-1,3),type="n",axes=FALSE)
	box()
	axis(2)
	axis(1,at=c(0,1,2))
	symbols(x=0:2,y=a,circles=0.2*sqrt(HWE)/(2*pi),bg=adjustcolor("blue",0.2),add=TRUE)  #ylim=range(a)*c(.2,1.8),xlim=c(-1,3)
	points(x=0:2,y=a,pch=19,cex=1)
	genos<-rbinom(10000,2,p)
	names(a)<-c(0,1,2)
	phenos<-a[as.character(genos)]
	abline(lm(phenos~genos),col="red",lwd=3)

}