
culex<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Culex_resistance/culex_resistance.csv")

layout(t(1:2))
 plot(culex[,c("dist_km_Ace1","Ace1_freq")],cex=1.2,pch=19,xlab="Distance from coast (km)",ylab="Allele frequency",ylim=c(.1,1),cex.axis=1.2,cex.lab=1.4,main="Ace 1",cex.main=1.4)
my.lowess<-lowess(culex[,c("dist_km_cline_Ace1","fitted_Ace1_freq")],f=1/7)
 lines(my.lowess$x,my.lowess$y,lwd=2) 
abline(v=19,col="black",lty=2)
text(x=10,y=.15, "Treated",cex=1.2)
text(x=30,y=.15, "Untreated",cex=1.2)

  plot(culex[,c("dist_km_Ester","Ester_freq")],cex=1.2,pch=19,xlab="Distance from coast (km)",ylab="Allele frequency",ylim=c(.1,1),,cex.axis=1.2,cex.lab=1.4,main="Ester",cex.main=1.4)
my.lowess<-lowess(culex[-29,c("dist_km_cline_Ester","fitted_Ester_freq")],f=1/7)
 lines(my.lowess$x,my.lowess$y,lwd=2)
 abline(v=19,col="black",lty=2)
text(x=10,y=.15, "Treated",cex=1.2)
text(x=30,y=.15, "Untreated",cex=1.2)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Culex_resistance/culex_resistance.pdf")