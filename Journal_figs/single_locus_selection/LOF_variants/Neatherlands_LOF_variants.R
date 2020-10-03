
### https://www.nature.com/articles/ng.3021

library(HistogramTools)

LOF<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/LOF_variants/Neatherlands_LOF_variants.csv")
layout(1)
#layout(t(1:2))
plot(PreBinnedHistogram(breaks=LOF$breaks,counts=LOF$counts[-nrow(LOF)]),freq=TRUE,xlab="# LOF alleles",ylab="Number of Individuals",main="",axes=FALSE,cex.lab=1.4)
axis(2)
axis(side=1,at=(12:17)*10)

mean.LOF<-sum((LOF$breaks[-nrow(LOF)] + diff(LOF$breaks)/2)* LOF$counts[-nrow(LOF)])/sum( LOF$counts[-nrow(LOF)])

abline(v=mean.LOF,col="red",lwd=2)
 

my_cuts<-cut(120:180, LOF$breaks)
tapply(dpois(120:180,mean.LOF)*sum( LOF$counts[-nrow(LOF)]),my_cuts,sum)

binned_counts<-tapply(dpois(120:180,mean.LOF)*sum( LOF$counts[-nrow(LOF)]),my_cuts,sum)


mean.fitness<-sum((1-10^(-2))^LOF$breaks[-nrow(LOF)]* LOF$counts[-nrow(LOF)])/sum( LOF$counts[-nrow(LOF)])


LOF.fitness<-(1-10^(-2))^LOF$breaks[-nrow(LOF)]

#Dropped this second graph
#plot(LOF$breaks[-nrow(LOF)],LOF.fitness/mean.fitness,type="b",xlab="# LOF alleles",cex.lab=1.2,ylab="Relative Fitness")
#abline(h=1,col="red")

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/LOF_variants/Neatherlands_LOF_variants.pdf")
