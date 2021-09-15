
##data from Frankam version of Soule (1976).  https://www.semanticscholar.org/paper/Relationship-of-Genetic-Variation-to-Population-in-Frankham/996bf4237263aa581fefd28b818692f336d0bafe
layout(t(1:2))
par(mar=c(4.5,4.5,0.5,0.5))

#https://www.biorxiv.org/content/10.1101/2020.04.07.030155v1
birds.1<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/island_birds/island_birds_range_size_pi.csv")

my.pch<-c(19,22)
names(my.pch)<-c("M","I")
plot(log10(birds.1$range_size),birds.1$piS,cex.axis=1.5,cex.lab=1.8,pch=my.pch[birds.1$Island_mainland],cex=2,lwd=2,xlab="log10 Range size",ylab="H")
legend(x="topleft",pch=my.pch,legend=c("Island","Mainland"),cex=2)

Allozyme<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Allozyme_pop_size/allozyme_pop_size.csv")
plot(Allozyme,pch=19,xlab="log N","H",cex.axis=1.5,cex.lab=1.8,cex=2,col=adjustcolor("black",0.7))
#dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Allozyme_pop_size/allozyme_pop_size.pdf")

dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Allozyme_pop_size/bird_allozyme_pop_size.pdf")
