library("latex2exp")

Galtier_dNdS<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/Galtier_dNdS/Galtier_dNdS.csv")
plot(x=range(Galtier_dNdS$log10_piS),y=range(Galtier_dNdS$dNdS),type="n",
xlab=TeX('$\\log_{10}(\\pi_S)$'),ylab="dN/dS",cex.lab=1.4,cex.axis=1.2,cex=1.2)

points(x=Galtier_dNdS$log10_piS[Galtier_dNdS$comp=="invertebrates"], Galtier_dNdS$dNdS[Galtier_dNdS$comp=="invertebrates"],col=adjustcolor("black"),pch=19)
#text(log10(1799	/268247),(7641/	1064230)/(16432/293023), "cuttlefish")

points(x=Galtier_dNdS$log10_piS[Galtier_dNdS$comp=="vertebrates"], Galtier_dNdS$dNdS[Galtier_dNdS$comp=="vertebrates"],cex=1.2)
legend(x="topright",legend=c("invertebrates","vertebrates"),pch=c(19,1),cex=1.2)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/Galtier_dNdS/Galtier_dNdS.pdf")

