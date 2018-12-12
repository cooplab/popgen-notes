

layout(t(1:2))
Dros_tajimas<-read.table("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/Andolfatto_subs_vs_dN/tajd.txt",as.is=TRUE)
 z<-Dros_tajimas
 rec=(z$V1)
  tajd=z$V2
 d_dmin=(z$V4)

plot(rec, d_dmin, ylim=c(-0.95,0.95), xlab="rec rate (cM/Mb)",ylab="Tajima's D",col=adjustcolor("black",0.7),cex=1.2,cex.axis=1.2,cex.lab=1.4,pch=19)
abline(0,0, lty = "dotted", lwd=1.5)
 lines(lowess(rec, d_dmin, f=1/3, iter=3), col = "red", lwd=3)

Andolfatto<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/Andolfatto_subs_vs_dN/Andolfatto_subs_vs_dN.csv")
plot(Andolfatto,pch=19,col=adjustcolor("black",0.7),cex=1.2,xlab="dN",ylab=TeX("$\\pi_S$"),cex.axis=1.2,cex.lab=1.4)
 lines(lowess(Andolfatto, f=1/3, iter=3), col = "red", lwd=3)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/Andolfatto_subs_vs_dN/Tajimas_D_subs_vs_dN.pdf")