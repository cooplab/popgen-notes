

Wright_Tajima_D<-read.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Maize_bottleneck/Wright_Tajima_D.csv")


counts<-Wright_Tajima_D[,c(3,5)]
counts[counts[,1]<0,1]<-0
counts[counts[,2]<0,2]<-0



layout(t(1:2))
plot(wright_data$pi_teosinte,wright_data$pi_maize,pch=19,col=adjustcolor("black",0.3),xlab="",ylab="",cex.lab=1.4)
abline(0,1,col="red",lwd=2)
mtext(side=2,expression(paste("Maize ", pi[bp])),line=2.5,cex=1.4)
mtext(side=1,expression(paste("Teosinte ", pi[bp])),line=2.5,cex=1.4)
barplot(as.matrix(t(counts)),beside=TRUE, names.arg=Wright_Tajima_D[,1],ylab="Counts",xlab="Tajima's D bin",cex.lab=1.4, legend.text= c("maize","teosinte"), args.legend=list(cex=1.5))
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Maize_bottleneck/Wright_Tajima_D.pdf")
