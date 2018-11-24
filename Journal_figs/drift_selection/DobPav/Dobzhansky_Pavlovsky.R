

DobPav<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/DobPav/Dobzhansky_Pavlovsky.csv")
large_DobPav<-DobPav[,grep("arge",colnames(DobPav))]
small_DobPav<-DobPav[,grep("Small",colnames(DobPav))]


par(mar=c(3,4.5,1,1.5))
layout(t(1:2))

plot(c(1,3),c(0,50),type="n",axes=FALSE,xlab="",ylab="Allele Frequency",cex.lab=1.4)
axis(2,cex.axis=1.2)
axis(1,at=1:3,lab=c("Start","4 months","6 months"),cex.axis=1.2)
apply(large_DobPav,1,function(timepoints){lines(c(50,timepoints),type="b")})
legend(x="bottomleft",leg="Large Populations",cex=1.4,bty="n")

plot(c(1,3),c(0,50),type="n",axes=FALSE,cex.lab=1.4,xlab="",ylab="")
axis(2,cex.axis=1.2)
axis(1,at=1:3,lab=c("Start","4 months","17 months"),cex.axis=1.2)
apply(small_DobPav,1,function(timepoints){lines(c(50,timepoints),type="b")})
legend(x="bottomleft",leg="Small Populations",cex=1.4,bty="n")

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/DobPav/Drift_sel_Dobzhansky_Pavlovsky.pdf")