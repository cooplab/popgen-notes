Neand_LD<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/alleles_genotypes/Neanderthal_LD/European_neanderthal_LD.csv")

plot(Neand_LD$cM_bin,Neand_LD$LD,pch=19,col=adjustcolor("black",0.6),xlab="Genetic distance (cM)",ylab="Neanderthal LD",cex.lab=1.4)

nls(LD ~ A*exp(-B*cM_bin),data=Neand_LD,start=list(A=1,B=0.5))

 my_x<-seq(0,1,length=100)
lines( my_x, 0.06724*exp(-11.87360* my_x),col="red",lwd=2)

dev.copy2pdf(file="Journal_figs/alleles_genotypes/Neanderthal_LD/European_neanderthal_LD.pdf")