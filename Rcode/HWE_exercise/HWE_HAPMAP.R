
#Made by using plink on the genotype file:PLINK
#plink --bfile CEU_YRI_r21_nr_all --hardy &
#mv plink.hwe CEU_YRI.hw
#gzip all_CEU_YRI.hw

library(RColorBrewer)
my.col<-brewer.pal(n=3, name="Dark2")

##paste me in
plot.geno.vs.HW<-function(file,title,cex.lab=1){
	
	#read in the HW file from plink
	plink.hwe<-read.table(file,as.is=TRUE) 

	names(plink.hwe)<-c("chr","SNP.id","which.inds","allele.1","allele.2","genotype","obs.het","exp.het","HWE.pval")

	counts<-sapply(plink.hwe$genotype,function(x){as.numeric(strsplit(x,"/")[[1]])})
	counts<-t(counts)
	tot.counts<-rowSums(counts)
	geno.freq<-counts/tot.counts
	allele.freq<-(geno.freq[,1]+.5*geno.freq[,2])

##alleles are ordered by minor allele, to make this prettier I flip 1/2 the alleles around
	these.minor<-1:(round(nrow(plink.hwe)/2))
	these.major<-(1+round(nrow(plink.hwe)/2)):nrow(plink.hwe)
	ss.allele<-c(allele.freq[these.minor],1-allele.freq[these.major])  
	ss.geno<-rbind(geno.freq[these.minor,],geno.freq[these.major,c(3,2,1)])


	plot(ss.allele,ss.geno[,1],xlim=c(0,1),ylim=c(0,1),col=my.col[1],xlab="allele frequency",ylab="genotype frequency",main=title,cex.lab=cex.lab)#adjustcolor(my.col[1],0.1),xlab="allele frequency",ylab="genotype frequency",main=title,cex.lab=cex.lab)
	points(ss.allele,ss.geno[,3],xlim=c(0,1),ylim=c(0,1),col=my.col[2]) #adjustcolor(my.col[2],0.1))
	points(ss.allele,ss.geno[,2],xlim=c(0,1),ylim=c(0,1),col=my.col[3])# adjustcolor(my.col[3],0.1))
	smooth=1/5
	lines(lowess(ss.geno[,1]~ss.allele,f = smooth),col="black")
	lines(lowess(ss.geno[,3]~ss.allele,f = smooth),col="black")
	lines(lowess(ss.geno[,2]~ss.allele,f = smooth),col="black")

	x=1:1000/1000
	lines(x,x^2,lty=2)
	lines(x,2*x*(1-x),lty=2)
	lines(x,(1-x)^2,lty=2)
	legend(x=0.2,y=1,col=c(my.col,rep("black",2)),legend=c("Homozygote AA","Homozygote aa","Heterozygote Aa","Mean","Hardy Weinberg Expectation"),pch=c(rep(1,3),rep(NA,2)),lty=c(rep(NA,3),1,2))
}


############
layout(t(1:2))
 plot.geno.vs.HW(file="YRI_10000.hw.gz",title="HapMap YRI (Africans)")
 plot.geno.vs.HW(file="CEU_10000.hw.gz",title="HapMap CEU (Europeans)")
 dev.copy2eps(file="../../Figs/CEU_YRI_separately_HWE.eps")

plot.geno.vs.HW(file="CEU_YRI_10000.hw.gz",title="Combined HapMap CEU + YRI (Europeans+Africans)")


##Run me 1st
#png(file="YRI_HWE.png")
plot.geno.vs.HW(file="YRI_10000.hw.gz",title="HapMap YRI (Africans)")
#dev.off()

###Run me 2nd
#png(file="CEU_HWE.png")
plot.geno.vs.HW(file="CEU_10000.hw.gz",title="HapMap CEU (Europeans)")
#dev.off()


####Run me third
#png(file="CEU_YRI_HWE.png")
plot.geno.vs.HW(file="CEU_YRI_10000.hw.gz",title="Combined HapMap CEU + YRI (Europeans+Africans)")
#dev.off()

#dev.copy2pdf(file="walund_effect_CEU_YRI.pdf")

##NSF grant figure
png(file="~/Dropbox/grants_and_assessments/2012/NSF_full_proposal/Figs/HWE_broader_impacts.png",width = 920, height = 480)
layout(t(1:2))
plot.geno.vs.HW(file="YRI_10000.hw.gz",title="HapMap YRI (Africans)")
plot.geno.vs.HW(file="CEU_YRI_10000.hw.gz",title="Combined HapMap CEU + YRI (Europeans+Africans)")
dev.off()
#

