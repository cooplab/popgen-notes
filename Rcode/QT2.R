

##Paste this in first
par.off.corr<-function(L=20, environ.var,Num_inds=1000,print.slope=FALSE,sel.cutoff=FALSE){
	##Quantitative genetics sims
	allele.freq<-0.5   ###each locus is assumed to have the same allele frequencies. This is just to simplify the coding, in reality these results work when each locus has its own frequency (and the coding wouldn't be too much harder). 
	 
	 
	
	##MAKE A MUM
	## For each mother, at each locus we draw an allele (either 0 or 1) from the population allele frequency. 
	##We do this twice for each mother two represent the two haplotypes in the mother 
	mum.hap.1<-replicate(Num_inds, rbinom(L,1,allele.freq) )
	mum.hap.2<-replicate(Num_inds, rbinom(L,1,allele.freq) )
	##type mum.hap.1[,1] to see the 1st mothers 1st haplotype
	
	##Each mothers genotype at each locus is either 0,1,2
	mum.geno<-mum.hap.1+mum.hap.2
	
	additive.genetic<-colSums(mum.geno)
	genetic.sd<-sd(additive.genetic)
	mean.genetic<-mean(additive.genetic)
	
	additive.genetic<-additive.genetic / sd(additive.genetic)
	mum.pheno<- additive.genetic + rnorm(Num_inds,sd=sqrt(environ.var))
	mum.pheno<-mum.pheno-mean(mum.pheno)
	
	
	
	##MAKE A DAD (same code as make a mum, only said in a deeper voice)
	dad.hap.1<-replicate(Num_inds, rbinom(L,1,allele.freq) )
	dad.hap.2<-replicate(Num_inds, rbinom(L,1,allele.freq) )
	dad.geno<-dad.hap.1+dad.hap.2
	
	
	additive.genetic<-colSums(dad.geno)
	additive.genetic<-additive.genetic / sd(additive.genetic)
	dad.pheno<- additive.genetic + rnorm(Num_inds,sd=sqrt(environ.var))
	dad.pheno<-dad.pheno-mean(dad.pheno)
	
	### Make a child
	child.geno<-dad.hap.1+mum.hap.1 ##1 haplotype from mum 1 haplotype from dad
	
	additive.genetic<-colSums(child.geno)
	additive.genetic<-additive.genetic / sd(additive.genetic)
	child.pheno<- additive.genetic + rnorm(Num_inds,sd=sqrt(environ.var))
	child.pheno<-child.pheno-mean(child.pheno)
	
	
	
	##Calculate midpoints, linear model and plots
	
	parental.midpoint<-(mum.pheno+dad.pheno)/2 ##avg. parents
	
	lm(child.pheno~parental.midpoint) ##linear model between child and mid point
								##the slope of this line is the narrow sense heritability.
	
	# plot parental midpoint against offsprings phenotype.
	#layout(1) ###done in case this is run after the code with 3 plots
	if(sel.cutoff){
		plot(parental.midpoint,child.pheno,xlab="Parental midpoint",ylab="Child's phenotype",cex=1.5,cex.axis=1.5,cex.main=1.5,cex.lab=1.5,main=paste("L =",L,"VE=",environ.var,", VA=1"),col=ifelse(parental.midpoint>1,"red","grey"))
	}else{
		plot(parental.midpoint,child.pheno,xlab="Parental midpoint",ylab="Child's phenotype",cex=1.5,cex.axis=1.5,cex.main=1.5,cex.lab=1.5,main=paste("L =",L,"VE=",environ.var,", VA=1"))
	}
	## plot the regression in red
	abline(h=0,col="grey",lwd=2)
	abline(v=0,col="grey",lwd=2)
	 abline(lm(child.pheno~parental.midpoint),col="blue",lwd=2)
	
	
	
	my.slope<-lm(child.pheno~parental.midpoint)$coeff[2]
	
	if(sel.cutoff){
		sel.child.mean<-mean(child.pheno[parental.midpoint>1])
		sel.par.mean<-mean(parental.midpoint[parental.midpoint>1]);
		points(sel.par.mean,0,col="blue",pch=19,cex=1.2)
		pred.sel.child<-my.slope*sel.par.mean
		points(0,pred.sel.child,col="blue",pch=19,cex=1.2)
		lines(c(0,sel.par.mean), rep(pred.sel.child,2),col="blue",lwd=2)
		lines(rep(sel.par.mean,2), c(0,pred.sel.child),col="blue",lwd=2)
		arrows(x0=0,x1=0,y0=0,y1=pred.sel.child,col="blue",lwd=2,length=0.1,code=3)
		arrows(x0=0,x1=sel.par.mean,y0=0,y1=0,col="blue",lwd=2,length=0.1,code=3)
		text(x=sel.par.mean/2,y=min(child.pheno)*0.06,"S",cex=1.5,col="blue")
		text(x=min(parental.midpoint)*0.06,y=pred.sel.child/2,"R",cex=1.5,col="blue")
	}
	
	if(print.slope) text(x=min(parental.midpoint)*.8,y=max(child.pheno)*.9,label=paste("slope= ",format(my.slope,digit=3)),col="red",lwd=4,cex=1.5)
	
	abline(0,1,col="red",lwd=3,lty=2)
 	recover()
 }
 

layout(t(1:3))
par.off.corr(L=100, environ.var=100,Num_inds=500)  #,print.slope=TRUE)
par.off.corr(L=100, environ.var=1,Num_inds=500) #,print.slope=TRUE)
par.off.corr(L=100, environ.var=0.001,Num_inds=500) #,print.slope=TRUE)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/QT.pdf")



par.off.corr(L=100, environ.var=1,Num_inds=500, sel.cutoff=1) #,print.slope=TRUE)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Breeders_eqn.pdf")


#for(environ.var in c(0.00001,0.5,1,2)){
#png(file=paste("parent_offspring_regression_varg_1_vare_",format(environ.var,dig=1),".png",sep=""))
# par.off.corr(L=200, environ.var=environ.var,print.slope=TRUE)
#dev.off()
# }
 

 
 