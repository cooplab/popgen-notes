
Neale_height<-read.table("~/Dropbox/Giant-Biobank/Datasets/UKB_neale_gwas/neale.height.best.snp.tsv.gz", sep="\t",head=TRUE)

Neale_height$freq<-Neale_height$AC/(2*Neale_height$nCompleteSample)



sim_UKbiobank<-replicate(1000,{
	sapply(Neale_height$freq,function(p){ rbinom(n=1,size=2,prob=p)})
})


sim.UKBB.heights <- colSums ( sim_UKbiobank * Neale_height$beta )

Neale_height$up_allele_freq <- Neale_height$freq
flip.these <- Neale_height$beta < 0
Neale_height$up_allele_freq[flip.these] <-  1 - Neale_height$up_allele_freq[flip.these]


sim_up_UKbiobank<-replicate(1000,{
	sapply(Neale_height$up_allele_freq,function(p){ rbinom(n=1,size=2,prob=p)})
})

layout(t(1:2))
hist(colSums(sim_up_UKbiobank),xlab="Number of Alleles assoc. with increase height",cex.lab=1.4,cex.ax=1.2,main="",ylab="count")
hist(sim.UKBB.heights, xlab="Height Polygenic Score",cex.lab=1.4,cex.ax=1.2,main="",ylab="count")
 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Biobank_height_dist.pdf")
 