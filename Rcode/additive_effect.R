#library("symbols")

plot.lm.genos<-function(a,p,dom.arrows=FALSE){
	HWE<-c((1-p)^2,2*p*(1-p),p^2)
	genos<-rbinom(10000,2,p)  #generate some inds for regression etc
	names(a)<-c(0,1,2)
	phenos<-a[as.character(genos)]
	pop.mean<-mean(a[as.character(genos)])	
	
	#symbols(x=0:2,y=a-sum(a*HWE),circles=HWE,ylim=range(a-sum(a*HWE)))
	plot(y=range(a)*c(.5,1.5)-pop.mean,x=c(-0.5,2.5),type="n",axes=FALSE)
	box()
	axis(2)
	axis(1,at=c(0,1,2))
	symbols(x=0:2,y=a-pop.mean,circles=3*sqrt(HWE)/(2*pi),bg=adjustcolor("blue",0.2),add=TRUE,inches=FALSE)  #ylim=range(a)*c(.2,1.8),xlim=c(-1,3)
	points(x=0:2,y=a-pop.mean,pch=19,cex=1)
	abline(lm((phenos-pop.mean)~genos),col="red",lwd=3)
	slope<-lm((phenos-pop.mean)~genos)$coeff
	z_bars<- a -pop.mean

	if(dom.arrows){
		to.line<-slope[1]+slope[2]*c(0,1,2)
		arrows(x0=0,x1=0,y0=z_bars["0"],y1=to.line[1],length=abs(z_bars["0"]-to.line[1])*.5)
		arrows(x0=1,x1=1,y0=z_bars["1"],y1=to.line[2],length=abs(z_bars["1"]-to.line[2])*.5)
		arrows(x0=2,x1=2,y0=z_bars["2"],y1=to.line[3],length=abs(z_bars["2"]-to.line[3])*.5)
	}

	a1<-z_bars["1"]*(1-p)+z_bars["2"]*p
 	a2<-z_bars["1"]*p+z_bars["0"]*(1-p)
 	print(slope)
 	points(x=0:2,y=c(a2*2,a1+a2,a1*2),pch=19,cex=1,col="red")
 	cat("average effect of gene subs. =",a1-a2,"\n")
#recover()
}


layout(matrix(1:4,nrow=2,byrow=TRUE))
par(mar=c(2,2,1,1))
a<-c(0.5,1,1.5)  ##additive
plot.lm.genos(a=a,p=0.1,dom.arrows=TRUE)
plot.lm.genos(a,p=0.9,dom.arrows=TRUE)
a<-c(0.5,1,1)  ##dominant
plot.lm.genos(a,p=0.1,dom.arrows=TRUE)
plot.lm.genos(a,p=0.9,dom.arrows=TRUE)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/additive_effect.pdf")

####BALANCING SELECTION
layout(t(1:3))
par(mar=c(2,2,1,1))

a<-c(0.5,1,0.75)
p.eq<-(1-a[1])/sum(1-a[c(1,3)])

plot.lm.genos(a=a,p=0.1,dom.arrows=TRUE)
plot.lm.genos(a=a,p=p.eq,dom.arrows=TRUE)
plot.lm.genos(a=a,p=0.9,dom.arrows=TRUE)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/additive_effect_OverDom.pdf")

mice.coat<-matrix(NA,nrow=3,ncol=3,dimname=list(c("MM","Mm","mm"),c("AA","Aa","aa")))
mice.coat["MM",]<-c(0,0,0)
mice.coat["Mm",]<-c(0.9,1,1.5)
mice.coat["mm",]<-c(1,1,1.9)
plot(c(0,1,2),mice.coat["MM",],ylim=range(mice.coat),col="blue",pch=19,type="b")
points(c(0,1,2),mice.coat["Mm",],col="purple",pch=19,type="b")
points(c(0,1,2),mice.coat["mm",],col="red",pch=19,type="b")

plot.lm.two.locus<-function(a,p1,p2,dom.arrows=FALSE){
	HWE.1<-c((1-p1)^2,2*p1*(1-p1),p1^2)
	HWE.2<-c((1-p2)^2,2*p2*(1-p2),p2^2)
	
	genos.1<-rbinom(10000,2,p1)  #generate some inds for regression etc
	genos.2<-rbinom(10000,2,p2)  #generate some inds for regression etc

	#names(a)<-c(0,1,2)
	#phenos<-a[as.character(genos)]
	pop.mean<-0  #mean(a[as.character(genos)])	
	
	#symbols(x=0:2,y=a-sum(a*HWE),circles=HWE,ylim=range(a-sum(a*HWE)))
	plot(y=range(a)*c(.5,1.5)-pop.mean,x=c(-0.5,2.5),type="n",axes=FALSE)
	box()
	axis(2)
	axis(1,at=c(0,1,2))
	my.col<-c("red","purple","blue")
sapply(1:3,function(i){
	symbols(x=0:2,y=a[i,]-pop.mean,circles=3*sqrt(HWE.1[i]*HWE.2)/(2*pi),bg=adjustcolor(my.col[i],0.3),add=TRUE,inches=FALSE)  #ylim=range(a)*c(.2,1.8),xlim=c(-1,3)
})

	points(x=0:2,y=a-pop.mean,pch=19,cex=1)
	abline(lm((phenos-pop.mean)~genos),col="red",lwd=3)
	slope<-lm((phenos-pop.mean)~genos)$coeff
	z_bars<- a -pop.mean
}
