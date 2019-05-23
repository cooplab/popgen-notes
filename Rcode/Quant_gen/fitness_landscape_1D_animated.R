
num.points<-1000
x<-seq(-10,40,length=num.points); y1<-dnorm(x,14,3.5)*1.8; y2<-dnorm(x,5,2); y3<-dnorm(x,19,sd=1)*0.25; 
my.y<-y1+y2+y3; 

d<-x
fitness.ind.surf<-my.y
sd<-2 #sd=4


calc.wbar<-function(d,sd,fitness.ind.surf){
wbar<-sapply(d,function(my.xbar){
	my.norm<-dnorm(d,mean=my.xbar,sd=sd)
	mean(my.norm/mean(my.norm)*fitness.ind.surf)
}
)
wbar
}


gif.my.fitness(d=d,fitness.ind.surf=fitness.ind.surf,my.mean=20,sd=2,n.gens=26,direct="~/Downloads/",file_prefix="selection_surf_1d_right_",xrange=c(0,25))


gif.my.fitness(d=d,fitness.ind.surf=fitness.ind.surf,my.mean=1,sd=2,n.gens=20,direct="~/Downloads/",file_prefix="selection_surf_1d_left_",xrange=c(0,25))


layout(matrix(1:6,nrow=2),heights=c(.4,0.9))
par(mar=c(1,4,1,1))	
plot.fitness.landscape(d=d,fitness.ind.surf=fitness.ind.surf,my.mean=1,sd=2,xrange=c(-5,25),wbar=NULL,add.legend=TRUE)
par(mar=c(1,4,1,1))
plot.fitness.landscape(d=d,fitness.ind.surf=fitness.ind.surf,my.mean=5.6,sd=2,xrange=c(-5,25),wbar=NULL,add.legend=FALSE)
par(mar=c(1,4,1,1))
plot.fitness.landscape(d=d,fitness.ind.surf=fitness.ind.surf,my.mean=20,sd=2,xrange=c(-5,25),wbar=NULL,add.legend=FALSE)


gif.my.fitness<-function(d,fitness.ind.surf,n.gens=20,my.mean=18,sd=2,xrange=c(1,25),direct="~/Downloads/",file_prefix="selection_surf_1d"){
	
	wbar<-calc.wbar(d=d,sd=sd,fitness.ind.surf=fitness.ind.surf)	
		
	for(gen in 1:n.gens){
		png(file=paste(direct,file_prefix, letters[gen], ".png",sep=""))
		layout(1:2,heights=c(.4,0.9))
		par(mar=c(1,4,1,1))	
		my.mean<-plot.fitness.landscape( d=d,fitness.ind.surf=fitness.ind.surf,my.mean=my.mean,sd=2,xrange=c(1,25),wbar=wbar)

		dev.off()
	
	}
	
	system(paste("convert -delay 200 $(ls -v ", direct, file_prefix,"*.png) ", direct,file_prefix, "output.gif",sep=""))
}

plot.fitness.landscape<-function( d=d,fitness.ind.surf=fitness.ind.surf,my.mean=20,sd=2,xrange=c(1,25),wbar=NULL,add.legend=TRUE){
		if(is.null(wbar)) wbar<-calc.wbar(d=d,sd=sd,fitness.ind.surf=fitness.ind.surf)
		my.norm<-dnorm(d,mean=my.mean,sd=sd)
		plot(d,my.norm,xlim=xrange,type="l",axes=FALSE,ylab="")
		mtext("Counts",side=2,line=1,cex=1.4)
		polygon(x=c(d,max(d),0),y=c(my.norm,0,0),col=adjustcolor("red",0.2))
		post.fit<-my.norm*fitness.ind.surf  #/sum(my.norm*fitness.ind.surf)
		lines(d,post.fit)	
		polygon(x=c(d,max(d),0),y=c(post.fit,0,0),col=adjustcolor("red",0.2))
	
		abline(v=my.mean,lwd=2)
		my.old.mean<-my.mean
		my.mean<-sum(my.norm*fitness.ind.surf*d)/sum(my.norm*fitness.ind.surf)
		abline(v=my.mean,lwd=2)
		#my.mean<-24.3
		#my.mean<-15
		
		if(add.legend) legend(x="topright",pch=15,col=c(adjustcolor("red",.2),adjustcolor("red",.4)),legend=c("All","Survivors"),bg="white",cex=1.2,pt.cex=1.2)
		par(mar=c(4,4,1,1))
	#	plot(galls,pch=19,xlab="Gall diameter",y="Proportion Surviving",cex.lab=1.4,cex.axis=1.2,cex=1.4,ylim=c(0,max(galls$proportion.surviving)))
		
		plot(d,fitness.ind.surf,xlim=xrange,lwd=3,type="l",xlab="",ylab="",axes=FALSE)
		mtext("Fitness",side=2,line=1,cex=1.4)
		mtext("Phenotype",side=1,line=1,cex=1.4)
		lines(d,wbar,lwd=2,lty=2)
		this.one<-which(d[-length(d)]< my.mean & d[-1] > my.mean)
		#abline(lm(galls$prop~galls$gall))
		abline(v=my.old.mean)
		my.probs<-dnorm(d,mean=my.mean,sd=sd)
		num.entries<-length(d)
		draws<-sample(1:num.entries,10000,replace=TRUE,prob=my.probs)
		my.lm<-lm(fitness.ind.surf[draws]~d[draws])
		#abline(my.lm,lty=1,col="red",lwd=2)
		segments(x0=my.old.mean-2,y0=(my.old.mean-2)*my.lm$coeff[2]+my.lm$coeff[1],x1= my.old.mean+2, y1=(my.old.mean+2)*my.lm$coeff[2]+my.lm$coeff[1],col="red",lwd=2)

#		legend(x="bottomright",lty=c(1,2,1,NA),pch=c(rep(NA,3),19),col=c("black","black","red","black"),legend=c("Fitness","Mean Fitness","Selection differential","Data"),bg="white")
		if(add.legend) legend(x="bottomright",lty=c(1,2,1),pch=c(rep(NA,3)),col=c("black","black","red"),legend=c("Fitness","Mean Fitness","Selection differential"),bg="white",cex=1.2,lwd=2)
		return(my.mean)
}	
