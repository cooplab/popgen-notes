  shapiro.data=read.table("~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/R code/shap_dat.txt")
  rec=  shapiro.data$V3
   pi=  shapiro.data$V4*100
 plot(rec, pi, xlab="rec rate (cM/Mb)", ylab="Syn diversity (%)",cex.axis=1.4,cex.lab=1.5)
 true.rec<-rec/10^8;
 nls(pi~neutral.pi*true.rec/(true.rec+alpha),start=list(neutral.pi=2,alpha=1e-9))
 lines(rec,2.75*true.rec/(true.rec+7.3e-9),col="red",lwd=3)

fig.direct="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/Figs/"

dev.copy2eps(file=paste(fig.direct,"Genomewide_HH.eps",sep=""))
