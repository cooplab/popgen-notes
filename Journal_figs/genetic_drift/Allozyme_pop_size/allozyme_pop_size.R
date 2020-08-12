
##data from Frankam version of Soule (1976).  https://www.semanticscholar.org/paper/Relationship-of-Genetic-Variation-to-Population-in-Frankham/996bf4237263aa581fefd28b818692f336d0bafe


Allozyme<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Allozyme_pop_size/allozyme_pop_size.csv")
plot(Allozyme,pch=19,xlab="log N","H",cex.lab=1.5,cex=1.4,col=adjustcolor("black",0.7))
dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Allozyme_pop_size/allozyme_pop_size.pdf")
