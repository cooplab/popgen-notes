
##Thanks to Pär Ingvarsson for sharing this data underlying Wang et al A major locus controls local adaptation and adaptive life history variation in a perennial plant

# Figure 6A https://genomebiology.biomedcentral.com/articles/10.1186/s13059-018-1444-y


aspen_budset<-read.table("Journal_figs/Quant_gen/Wang_GWAS_poplar/FT2_budset.txt",as.is=TRUE,head=TRUE)

plot(jitter(aspen_budset$ft2,amount=0.07),aspen_budset$budset,pch=19,col=adjustcolor("black",0.4),axes=FALSE,xlab="PtFT2 Genotype",ylab="Budset (days)",cex.lab=1.5,cex=1.2)

line.width<-c(-0.15,0.15)
sapply(0:2, function(geno){
	geno.mean<-mean(aspen_budset$budset[aspen_budset$ft2==geno])
	geno.sd<-sd(aspen_budset$budset[aspen_budset$ft2==geno])/sum(aspen_budset$ft2==geno)
	inter.quart<-quantile(aspen_budset$budset[aspen_budset$ft2==geno],p=c(0.25,0.75))
	lines(x=geno+line.width,rep(geno.mean,2),lwd=2)
	#lines(x=rep(geno,2),y=c(geno.mean+1.96*geno.sd,geno.mean-1.96*geno.sd),lwd=2)	
	lines(x=rep(geno,2),y=inter.quart,lwd=2)	
})
abline(lm(aspen_budset$budset~aspen_budset$ft2),lty=2)

axis(1,at=c(0,1,2),lab=c("TT","GT","GG"),cex.lab=1.5)
axis(2)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Wang_GWAS_poplar/Poplar_Aspen_budset_geno_pheno.pdf")

