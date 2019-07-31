

par(mar=c(1,4,1,1))
galls<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Weis_Gorman_gall_size_stablizing_sel/gall_size.csv")


##LOAD fitness_landscape_1D_animated.R

d<-seq(5,40,length=1000)
fitness.ind.surf<-0.21+0.42*exp(-0.5*((d-24.3)/3.84)^2)

layout(matrix(1:4,nrow=2),heights=c(.4,0.9))
plot.fitness.landscape(d=d,fitness.ind.surf=fitness.ind.surf,my.mean=18,sd=2,xrange=range(galls$gall.diameter),yrange=range(galls$proportion.surviving),wbar=NULL,add.legend=TRUE,model="quadratic",pheno.lab="Gall size (mm)")

points(galls,pch=19);
plot.fitness.landscape(d=d,fitness.ind.surf=fitness.ind.surf,my.mean=24.2,sd=2,xrange=range(galls$gall.diameter),yrange=range(galls$proportion.surviving),wbar=NULL,add.legend=FALSE,model="quadratic",pheno.lab="Gall size (mm)")
points(galls,pch=19)

 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Weis_Gorman_gall_size_stablizing_sel/gall_size.pdf")

###OLD CODE


wbar<-sapply(d,function(my.xbar){
	my.norm<-dnorm(d,mean=my.xbar,sd=4)
	mean(my.norm/mean(my.norm)*fitness.ind.surf)
}
)


direct<-"~/Downloads/"
file_prefix<-"gall_selection"
my.mean<-15

for(gen in 1:15){
	png(file=paste(direct,file_prefix, letters[gen], ".png",sep=""))
	layout(1:2,heights=c(.4,0.9))
	par(mar=c(1,4,1,1))
	
	my.norm<-dnorm(d,mean=my.mean,sd=4)
	plot(d,my.norm,xlim=range(galls$gall),type="l",axes=FALSE,ylab="Count")
	polygon(x=c(d,0),y=c(my.norm,0),col=adjustcolor("red",0.2))
	post.fit<-my.norm*fitness.ind.surf
	lines(d,post.fit)	
	polygon(x=c(d,0),y=c(post.fit,0),col=adjustcolor("red",0.2))

	abline(v=my.mean,lwd=2)
	my.old.mean<-my.mean
	my.mean<-sum(my.norm*fitness.ind.surf*d)/sum(my.norm*fitness.ind.surf)
	abline(v=my.mean,lwd=2)
	#my.mean<-24.3
	#my.mean<-15
	
	legend(x="topright",pch=15,col=c(adjustcolor("red",.2),adjustcolor("red",.4)),legend=c("All","Survivors"),bg="white")
	par(mar=c(4,4,1,1))
	plot(galls,pch=19,xlab="Gall diameter",y="Proportion Surviving",cex.lab=1.4,cex.axis=1.2,cex=1.4,ylim=c(0,max(galls$proportion.surviving)))
	
	lines(d,fitness.ind.surf,lwd=2)
	
	lines(d,wbar,lwd=2,lty=2)
	this.one<-which(d[-length(d)]< my.mean & d[-1] > my.mean)
	#abline(lm(galls$prop~galls$gall))
	abline(v=my.old.mean)
	my.probs<-dnorm(d,mean=my.mean,sd=4)
	num.entries<-length(d)
	draws<-sample(1:num.entries,10000,replace=TRUE,prob=my.probs)
	abline(lm(fitness.ind.surf[draws]~d[draws]),lty=1,col="red",lwd=2)
	legend(x="bottomright",lty=c(1,2,1,NA),pch=c(rep(NA,3),19),col=c("black","black","red","black"),legend=c("Fitness","Mean Fitness","Selection differential","Data"),bg="white")
	dev.off()

}


system(paste("convert -delay 200 $(ls -v ", direct, file_prefix,"*.png) ", direct,file_prefix, "output.gif",sep=""))
