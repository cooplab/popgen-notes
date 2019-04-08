

finches<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Darwins_Finches_unpred/Darwins_Finches_unpred.csv",as.is=TRUE)

layout(1:2)
par(mar=c(2,4,1,1))
plot(finches$year_pheno,finches$PC1_body_size,type="b",pch=19,lwd=2,ylab="",xlab="",cex.axis=1.2);
mtext("Mean body size",side=2,cex=1.4,line=2.9)
 abline(h=finches$PC1_body_size[1:2],col="grey")
 
par(mar=c(3,4,0,1))
my.shades<-c("white","lightgrey","lightgrey","grey","black"); names(my.shades)<-as.character(0:4) 
plot(finches$year_sel_difd,finches$PCA_body_size_sel_diff,bg=my.shades[as.character(finches$sel_diff_signif)],pch=21,ylab="",cex.axis=1.2,cex=1.5,type="n") 
 abline(h=0,col="grey")
points(finches$year_sel_difd,finches$PCA_body_size_sel_diff,bg=my.shades[as.character(finches$sel_diff_signif)],pch=21,cex=1.5)
mtext("Selection gradient",side=2,cex=1.4,line=2.9)


#Directional selection differentials were calculated as the difference in trait means before and after selection, then standardized in each case by divid- ing the difference by the standard deviation of the sample before selection 

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Darwins_Finches_unpred/Darwins_Finches_unpred.pdf")