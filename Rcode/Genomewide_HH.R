shapiro.data=read.table("~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/shap_dat.txt")
  rec=  shapiro.data$V3
   pi=  shapiro.data$V4*100
 plot(rec, pi, xlab="rec rate (cM/Mb)", ylab="Syn diversity (%)",cex.axis=1.4,cex.lab=1.5)
 true.rec<-rec/10^8;
 nls(pi~neutral.pi*true.rec/(true.rec+alpha),start=list(neutral.pi=2,alpha=1e-9))
 lines(rec,2.75*true.rec/(true.rec+7.3e-9),col="red",lwd=3)

fig.direct="~/Dropbox/Courses/Popgen_teaching_Notes/figures/"

dev.copy2eps(file=paste(fig.direct,"Genomewide_HH.eps",sep=""))
 
 
  plot(rec, pi, xlab="rec rate (cM/Mb)", ylab="Syn diversity (%)",cex.axis=1.4,cex.lab=1.5,pch=19,col=adjustcolor("black",0.6))

 nls(pi~neutral.pi*exp(-alpha/true.rec),start=list(neutral.pi=2,alpha=1e-9)) ##BGS model fitted.
 lines(sort(rec),2.75*sort(true.rec)/(sort(true.rec)+7.3e-9),col="blue",lwd=3,lty=2)
  lines(sort(rec),2.31*exp(-3.223e-09/sort(true.rec)),col="red",lwd=3)
  legend(x="topleft",legend=c("BGS","Hitchhiking"),lty=c(1,2),cex=1.2,col=c("red","blue"),lwd=3)
dev.copy2pdf(file=paste(fig.direct,"Genomewide_BGS_HH.pdf",sep=""))