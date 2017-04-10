

##Paste this in first
genetic.covar<-function(L=20, environ.var,Num_inds=1000,print.slope=FALSE,sel.cutoff=FALSE,ibd.prob,relly.type=""){
	##Quantitative genetics sims
	allele.freq<-0.5   ###each locus is assumed to have the same allele frequencies. This is just to simplify the coding, in reality these results work when each locus has its own frequency (and the coding wouldn't be too much harder). 
	 stopifnot(sum(ibd.prob)==1)
	 
	
	##MAKE A IND 1ss
	## For each ind, at each locus we draw an allele (either 0 or 1) from the population allele frequency. 
	##We do this twice for each ind two represent the two haplotypes in the mother 
	ind.hap.1<-replicate(Num_inds, rbinom(L,1,allele.freq) )
	ind.hap.2<-replicate(Num_inds, rbinom(L,1,allele.freq) )
	##type mum.hap.1[,1] to see the 1st mothers 1st haplotype
	
	##Each mothers genotype at each locus is either 0,1,2
	ind.geno<-ind.hap.1+ind.hap.2
	
	additive.genetic<-colSums(ind.geno)
	genetic.sd<-sd(additive.genetic)
	mean.genetic<-mean(additive.genetic)
	
	additive.genetic<-additive.genetic / sd(additive.genetic)
	ind.pheno<- additive.genetic + rnorm(Num_inds,sd=sqrt(environ.var))
	ind.pheno<-ind.pheno-mean(ind.pheno)

	##MAKE A IND 2's
	###routine to generate genotypes for set of 2nd individual based on our genotypes for first
	other.ind.geno<-sapply(1:Num_inds,function(ind){
		sapply(1:L,function(snp){
			num.ibd<-sample(0:2,1,prob=ibd.prob)
			if(num.ibd==0){my.geno<-sum(rbinom(2,1,allele.freq))} 
			if(num.ibd==1){my.geno<-ind.hap.1[snp,ind]+rbinom(1,1,allele.freq)} 
			if(num.ibd==2){my.geno<-ind.geno[snp,ind]}
			return(my.geno)
		})
	})
	
	other.ind.additive.genetic<-colSums(other.ind.geno)
	genetic.sd<-sd(other.ind.additive.genetic)
	mean.genetic<-mean(other.ind.additive.genetic)	
	other.ind.additive.genetic<-other.ind.additive.genetic / sd(other.ind.additive.genetic)
	other.pheno<- other.ind.additive.genetic + rnorm(Num_inds,sd=sqrt(environ.var))
	other.pheno<-other.pheno-mean(other.pheno)

	plot(ind.pheno,other.pheno,xlab="Ind 1's phenotype",ylab="Ind 2's phenotype",cex=1.5,cex.axis=1.5,cex.main=1.5,cex.lab=1.5,
	main=paste(relly.type,"L =",L,"VE=",environ.var,"VA=1",sep=", "))
	abline(h=0,col="grey",lwd=2)
	abline(v=0,col="grey",lwd=2)
	abline(lm(other.pheno~ind.pheno),col="blue",lwd=2)
	abline(0,1,col="red",lwd=3,lty=2)
	my.cov<-cov(ind.pheno,other.pheno);
	text(x=min(ind.pheno)*.7,y=max(other.pheno)*.9,label=paste("Cov= ",format(my.cov,digit=3)),col="red",lwd=4,cex=1.5)
	cat("pheno. covariance=",my.cov,"\n")
	cat("Expected covar=",sum(ibd.prob*c(0,0.5,1)),"\n")
	VA<-my.cov/sum(ibd.prob*c(0,0.5,1))
	cat("VA= ", VA,"h2= ",VA/var(c(ind.pheno,other.pheno)),"\n")
 }
 

layout(t(1:3))

genetic.covar(L=100, environ.var=0.1,Num_inds=500,ibd.prob=c(0.25,0.5,0.25),relly.type="Full Sibs")
genetic.covar(L=100, environ.var=0.1,Num_inds=500,ibd.prob=c(0.5,0.5,0),relly.type="1/2 Sibs")
genetic.covar(L=100, environ.var=0.1,Num_inds=500,ibd.prob=c(0.75,0.25,0.0),relly.type="1st Cousins")

 

 
 