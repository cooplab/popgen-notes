Scythian<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Scythian_horses/Scythian_horses.csv",as.is=TRUE)

Scythian<-Scythian[Scythian$pop %in% c("Berel","Nordic"),]

my.bars<-barplot(pi~pop+locus, data=Scythian,beside=TRUE,cex.names=1.5,cex.lab=1.8,cex.axis=1.3,xlab="",ylab=expression(pi), 
        legend.text = c("Scythian","Modern"),args.legend=list(cex=1.5)	
)


pi.ratios<-Scythian[Scythian$pop=="Nordic","pi"]/Scythian[Scythian$pop=="Berel","pi"]
names(pi.ratios)<-Scythian$locus[1:4]
pi.ratios<-pi.ratios[c("auto","mtDNA","X","Y")]
text(x=my.bars[2,]+0.2,y=Scythian[Scythian$pop=="Nordic","pi"][c(2,1,3,4)]+1e-4,format(pi.ratios,dig=2),cex=1.4)
dev.copy2pdf(file="Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Scythian_horses/Scythian_horses.pdf")

