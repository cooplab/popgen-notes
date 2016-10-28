

one.gen.sel<-function(L=1000,environ.var,sel,plot.geno=FALSE){
##Quantitative genetics sims
allele.freq<-0.5   ###each locus is assumed to have the same allele frequencies. This is just to simplify the coding, in reality these results work when each locus has its own frequency (and the coding wouldn't be too much harder). 
 

Num_inds=10000

##MAKE A MUM
## For each mother, at each locus we draw an allele (either 0 or 1) from the population allele frequency. 
##We do this twice for each mother two represent the two haplotypes in the mother 
mum.hap.1<-replicate(Num_inds, rbinom(L,1,allele.freq) )
mum.hap.2<-replicate(Num_inds, rbinom(L,1,allele.freq) )
##type mum.hap.1[,1] to see the 1st mothers 1st haplotype

##Each mothers genotype at each locus is either 0,1,2
mum.geno<-mum.hap.1+mum.hap.2

additive.genetic<-colSums(mum.geno)
mean.genetic<-mean(additive.genetic)
genetic.var<-sd(additive.genetic)

additive.genetic<-additive.genetic / sd(additive.genetic)
mum.pheno<- additive.genetic + rnorm(Num_inds,sd=sqrt(environ.var))
mum.pheno<-mum.pheno-mean(mum.pheno)



###FAMILIES


##MAKE A DAD (same code as make a mum, only said in a deeper voice)
dad.hap.1<-replicate(Num_inds, rbinom(L,1,allele.freq) )
dad.hap.2<-replicate(Num_inds, rbinom(L,1,allele.freq) )
dad.geno<-dad.hap.1+dad.hap.2


additive.genetic<-colSums(dad.geno)
additive.genetic<-additive.genetic / sd(additive.genetic)
dad.pheno<- additive.genetic + rnorm(Num_inds,sd=sqrt(environ.var))
dad.pheno<-dad.pheno-mean(dad.pheno)

### Make a child
child.geno<-dad.hap.1+mum.hap.1 ##1/2 from mum 1/2 from dad

additive.genetic<-colSums(child.geno)
additive.genetic<-additive.genetic / sd(additive.genetic)
child.pheno<- additive.genetic + rnorm(Num_inds,sd=sqrt(environ.var))
child.pheno<-child.pheno-mean(child.pheno)



 ##Selection of top sel% of individuals
 
top.sel.per.mums<- mum.pheno>quantile(mum.pheno,p=1-sel) 
top.sel.per.dads<- dad.pheno>quantile(dad.pheno,p=1-sel)
 
 
child.geno<-dad.hap.1[,top.sel.per.dads]+mum.hap.1[,top.sel.per.mums] ##1/2 from mum 1/2 from dad

additive.genetic<-(colSums(child.geno)-mean.genetic)
additive.genetic<-additive.genetic/genetic.var
child.pheno<- additive.genetic + rnorm(length(child.geno),sd=sqrt(environ.var))

layout(1:3)
my.lim<-quantile(c(mum.pheno,dad.pheno),p=c(0.01,0.99))
my.lim[2]<-quantile(child.pheno,p=c(0.99))

hist(c(mum.pheno,dad.pheno),breaks=100,xlim=my.lim,xlab="Phenotype",main=paste("Phenotype distribution before selection, Mean=0, VA=1, VE=",environ.var,", Taking top ",round(100*sel),"%",sep=""),cex.axis=1.5,cex.lab=1.5,cex.main=1.5); 
abline(v=0,col="blue",lwd=3)

hist(c(mum.pheno[top.sel.per.mums],dad.pheno[top.sel.per.dads]),breaks=100,xlim=my.lim,xlab="Phenotype",main=paste("Phenotype distribution after selection, parental mean=",format(mean(c(mum.pheno[top.sel.per.mums],dad.pheno[top.sel.per.dads])),dig=4)),cex.axis=1.5,cex.lab=1.5,cex.main=1.5); 
abline(v= mean(c(mum.pheno[top.sel.per.mums],dad.pheno[top.sel.per.dads])),col="red",lwd=3)
abline(v=0,col="blue",lwd=3)

hist(child.pheno,xlim=my.lim,breaks=100,xlab="Phenotype",main=paste("Phenotype distribution in the children Mean in children = ",format(mean(child.pheno),dig=4)),cex.axis=1.5,cex.lab=1.5,cex.main=1.5); 
abline(v=0,col="blue",lwd=3)
abline(v= mean(child.pheno),col="red",lwd=3)
##Mean phenotype after selection
cat("Selected parental mean",mean(c(mum.pheno[top.sel.per.mums],dad.pheno[top.sel.per.dads])),"\n")
##Mean child phenotype
cat("Mean in children = ",mean(child.pheno),"\n")

if(plot.geno){
#	quartz()
	layout(1:2)
	sel.dad.genosum<-colSums(dad.geno[,top.sel.per.dads])
	rand.dad.genosum<-colSums(dad.geno[,sample(top.sel.per.dads)])
	hist(rand.dad.genosum,breaks=20,  col = rgb ( 1 , 0 , 0 , 0.4 ),xlim=c(min(rand.dad.genosum)-5,max(sel.dad.genosum)+5),ylim=c(0,160),xlab="Num. tall alleles", main="Parental generation",cex.lab=1.5)
	hist(sel.dad.genosum,breaks=20,  col =rgb ( 0 , 0 , 1 , 0.4 ),add=TRUE)
	legend ( "topleft" , legend = c ( "All individuals" , "Selected parents" ) , pch = 15 , col = c ( rgb ( 1 , 0 , 0 , 0.4 )  , rgb ( 0 , 0 , 1 , 0.4 ) ) , bty = "n" , cex = 1.5 )
	###hist of kids
	hist(colSums(child.geno),breaks=20,  col =rgb ( 0 , 1 , 0 , 0.4 ),xlim=c(min(rand.dad.genosum)-5,max(sel.dad.genosum)+5),ylim=c(0,160),xlab="Num. tall alleles", main="Next generation",cex.lab=1.5)
	legend ( "topleft" , legend = c ( "Children" ) , pch = 15 , col = rgb ( 0 , 1 , 0 , 0.4 ), bty = "n" , cex = 1.5 )
}
}

one.gen.sel(L=100,environ.var=1,sel=0.1)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/QT3.pdf")
pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/QT3_w_genosums.pdf")
one.gen.sel(L=100,environ.var=1,sel=0.1,plot.geno=TRUE)
dev.off()

#for(sel in c(0.1,0.4))
#for(environ.var in c(.01,1,50)){
#png(file=paste("One_generation_selection_varg_1_vare_",format(environ.var,dig=1),"sel_",format(sel,dig=1),".png",sep=""))
#one.gen.sel(L=50,environ.var=environ.var,sel=sel) ##high herit.
#dev.off()
#}

