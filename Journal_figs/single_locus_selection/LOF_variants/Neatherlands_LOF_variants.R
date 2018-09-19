
### https://www.nature.com/articles/ng.3021

library(HistogramTools)

LOF<-read.csv("Journal_figs/single_locus_selection/LOF_variants/Neatherlands_LOF_variants.csv")


plot(PreBinnedHistogram(breaks=LOF$breaks,counts=LOF$counts[-nrow(LOF)]),freq=TRUE,xlab="# LOF alleles",ylab="Number of Individuals",main="",axes=FALSE,cex.lab=1.4)
axis(2)
axis(side=1,at=(12:17)*10)

mean.LOF<-sum((LOF$breaks[-nrow(LOF)] + diff(LOF$breaks)/2)* LOF$counts[-nrow(LOF)])/sum( LOF$counts[-nrow(LOF)])

abline(v=mean.LOF,col="red",lwd=2)
 

my_cuts<-cut(120:180, LOF$breaks)
tapply(dpois(120:180,mean.LOF)*sum( LOF$counts[-nrow(LOF)]),my_cuts,sum)

binned_counts<-tapply(dpois(120:180,mean.LOF)*sum( LOF$counts[-nrow(LOF)]),my_cuts,sum)

dev.copy2pdf(file="Journal_figs/single_locus_selection/LOF_variants/Neatherlands_LOF_variants.pdf")