
###USES First L1 of the L loci to make mum's trait & 2nd L2 to make Dad's trait
##VA is set at 1, so you control h^2 with Ve. The corr.coeff controls the assortative mating.


sample.initial.inds<-function(Num_inds, L, allele.freq){
	## For each ind, at each locus we draw an allele (either 0 or 1) from the population allele frequency. 
	##We do this twice for each mother two represent the two haplotypes in the mother 
	ind.hap.1<-replicate(Num_inds, rbinom(L,1,allele.freq) )
	ind.hap.2<-replicate(Num_inds, rbinom(L,1,allele.freq) )
	##type mum.hap.1[,1] to see the 1st mothers 1st haplotype
	
	##Each mothers genotype at each locus is either 0,1,2
	list(hap.1=ind.hap.1,hap.2= ind.hap.2)
}


make.a.kid.hap<-function(par.geno){
	kid.hap<-numeric(length(par.geno))
	kid.hap[par.geno == 0]<-0
	kid.hap[par.geno == 2]<-1
	kid.hap[par.geno == 1] <- sample(0:1,size=sum(par.geno == 1),replace=TRUE)
	kid.hap
}

assortative.mating<-function(mum.pheno,dad.pheno,corr.coeff){
	var.mums<-var(mum.pheno)
	mean.mums<-var(mum.pheno)
	##assortative mating step
	##ladies pick your partners
	##to induce the correct correlation
	##Given a female's phenotype we simulate her partner's phenotype from the conditional normal distribution 
	##We look to see which male has a phenotype closest to this simulated phenotype and then assign that male to the female. 
	these.dads<-sapply(mum.pheno,function(mum){
		mums.choice<- rnorm(1,mean=mum*corr.coeff,sd=sqrt(var.mums*(1-corr.coeff^2)))
		this.dad<-which.min(abs(dad.pheno-mums.choice))
	return(this.dad)
	}
	)
	these.dads
}

##Paste this in first
QT.assortative.mating<-function(L1,L2, environ.var,corr.coeff=0,par.environ.effec0,plots=FALSE,write.plots=FALSE,Num_inds=1e4){
	##Quantitative genetics sims
	allele.freq<-0.5   ###each locus is assumed to have the same allele frequencies. This is just to simplify the coding, in reality these results work when each locus has its own frequency (and the coding wouldn't be too much harder). 
	total.loci <- unique(c(L1,L2)) 
	L<-length(total.loci) 
#	recover() 
	genetic.sd.1<-  sqrt(length(L1)*2 *allele.freq*(1-allele.freq))
	genetic.sd.2<- sqrt(length(L2) *2 *allele.freq*(1-allele.freq))
	
	##MAKE A MUM
	mums.haps<-sample.initial.inds(Num_inds, L, allele.freq)	
	mum.geno <- mums.haps[["hap.1"]] + mums.haps[["hap.2"]]
	additive.genetic.1.mum <-   colSums(mum.geno[L1,])   
	additive.genetic.1.mum<-additive.genetic.1.mum/ genetic.sd.1
	mum.pheno<- additive.genetic.1.mum + rnorm(Num_inds,sd=sqrt(environ.var))
	mum.pheno<-mum.pheno-mean(mum.pheno)

	##MAKE A DAD (same code as make a mum, only said in a deeper voice)
	dads.haps<-sample.initial.inds(Num_inds, L, allele.freq)	
	dad.geno <- dads.haps[["hap.1"]] + dads.haps[["hap.2"]]
	additive.genetic.2.dad<-  colSums(dad.geno[L2,])
	additive.genetic.2.dad<-additive.genetic.2.dad / genetic.sd.2
	dad.pheno<- additive.genetic.2.dad + rnorm(Num_inds,sd=sqrt(environ.var))
	dad.pheno<-dad.pheno-mean(dad.pheno)

	these.dads<-assortative.mating(mum.pheno,dad.pheno,corr.coeff)	
	#now rearrange dads pheno and genos.
	dad.pheno<-dad.pheno[these.dads]
	dad.geno<- dad.geno[,these.dads]
	additive.genetic.2.dad<-additive.genetic.2.dad[these.dads]
	cor.1<-cor.test(mum.pheno,dad.pheno)$estimate
	cat("correlation coefficient between parental phenotypes",cor.1,"\n")
	cor.2<-cor.test(additive.genetic.1.mum,additive.genetic.2.dad)$estimate
	cat("correlation coefficient between parental additive phenotype",cor.2,"\n")

	
	if(plots){ 
		if(write.plots){ png(file="Pheno_assort_plot.png")} else{layout(t(1:2))}
		plot(mum.pheno,dad.pheno,xlab="Female Pheno.",ylab="Male Pheno.",col=adjustcolor("black",0.2),main=paste("Corr. parental pheno.=",corr.coeff))
	abline(lm(dad.pheno~mum.pheno),col="red")
		text(x=-2,y=2,format(cor(dad.pheno,mum.pheno),dig=2),col="red")
		if(write.plots) dev.off()
	}
	
	
	if(plots){
			if(write.plots){ png(file="Mean_geno_assort_plot.png")} 
		 plot(colSums(mum.geno)/L,colSums(dad.geno)/L,xlab="Female Mean Genotype",ylab="Male Mean Genotype",col=adjustcolor("black",0.2),,main=paste("Cor. parental pheno.=",corr.coeff,"Heritability",format(1/(1+environ.var),dig=2)))	
		dad.mean<-colSums(dad.geno)/L
		mum.mean<-colSums(mum.geno)/L
		abline(lm(dad.mean~mum.mean),col="red")
		text(x=0.5,y=1.5,format(cor(mum.mean,dad.mean),dig=2),col="red")
		if(write.plots) dev.off()
	}
	
	### Make a child
	
	###DO TWICE ONCE FOR DAUGHTER ONCE FOR SONS
	dad.hap.1<-apply(dad.geno,2 , make.a.kid.hap)
	mum.hap.1<-apply(mum.geno,2 , make.a.kid.hap)
	child.geno.1<-dad.hap.1+mum.hap.1 ##1 haplotype from mum 1 haplotype from dad
	dad.hap.2<-apply(dad.geno,2 , make.a.kid.hap)
	mum.hap.2<-apply(mum.geno,2 , make.a.kid.hap)
	child.geno.2<-dad.hap.2+mum.hap.2 ##1 haplotype from mum 1 haplotype from dad


#	cat(mean(child.geno==0),mean(child.geno==1),mean(child.geno==2),"\n")

	additive.genetic.pheno.1<-colSums(child.geno.1[L1,])   
	additive.genetic.pheno.2<-colSums(child.geno.2[L2,]) 
	additive.genetic.pheno.1<-additive.genetic.pheno.1 / genetic.sd.1
	additive.genetic.pheno.2<-additive.genetic.pheno.2 / genetic.sd.2
	child.pheno.1<- additive.genetic.pheno.1 + rnorm(Num_inds,sd=sqrt(environ.var))
	child.pheno.2<- additive.genetic.pheno.2 + rnorm(Num_inds,sd=sqrt(environ.var))
	##cor between same phenotype in parent and child
	
	layout(t(1:2))
	plot(mum.pheno,dad.pheno,xlab="Mother's pref. trait",ylab="Father's display trait",col=adjustcolor("black",0.4),cex.lab=1.5) 
	plot(additive.genetic.pheno.1- mean(additive.genetic.pheno.1),additive.genetic.pheno.2- mean(additive.genetic.pheno.2),xlab="Mean daughters' pref. trait",ylab="Mean sons' display trait",col=adjustcolor("black",0.4),cex.lab=1.5)
		
 }
 
QT.assortative.mating(L1=1:250, L2=26:500,environ.var=.1,corr.coeff=0.80,plots=FALSE,Num_inds=1000)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Response_to_sel/Genetic_corr_assort_mating.pdf")