

geomeans<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Gremer_hedging_example/DIgeomeans.csv")
a_means<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Gremer_hedging_example/DIameans.csv")
sds<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Gremer_hedging_example/DIsds.csv")

layout(t(1:2));par(mar=c(3,4,0.5,1))
species<-"plpa" #"scba"  #
plot(a_means[,species],lty=1,lwd=2,type="l",ylim=range(sds[,species]),cex.axis=1.2,xlab="",ylab="")
lines(sds[,species],lty=2,lwd=2)
mtext(side=2,text="Arithmetic Fitness",cex=1.4,line=2.5)
mtext(side=1,text="Proportion Germinating (per year)",cex=1.4,line=2)
legend(x="topleft",legend=c("Mean","Std. Dev"),lty=1:2,cex=1.4)

plot(geomeans[-100,species],type="l",lty=3,lwd=2,cex.axis=1.2,xlab="",ylab="")
mtext(side=1,text="Proportion Germinating (per year)",cex=1.4,line=2)
mtext(side=2,text="Geometric Mean Fitness",cex=1.4,line=2.5)
abline(v=which.max(geomeans[,species]),lwd=2,col="grey")

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Gremer_hedging_example/Gremer_hedging_example.pdf")