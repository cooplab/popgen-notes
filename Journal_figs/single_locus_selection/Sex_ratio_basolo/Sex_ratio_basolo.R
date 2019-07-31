


sex.ratio<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Sex_ratio_basolo/Sex_ratio_basolo.csv")

plot(c(0,3),y=c(0,1),type="n",xlab="Generation",ylab="Sex ratio (% males)",cex.lab=1.4,cex.axis=1.2)
abline(h=0.5,lwd=2,col="grey")

lines(sex.ratio[sex.ratio$replicate=="rep_1_down",c("generation","sex.ratio")],pch=19,type="b",lwd=2,cex=2)
lines(sex.ratio[sex.ratio$replicate=="rep_2_down",c("generation","sex.ratio")],pch=15,type="b",lwd=2,cex=2)
lines(sex.ratio[sex.ratio$replicate=="rep_1_up",c("generation","sex.ratio")],type="b",lty=2,lwd=2,cex=2,pch=21)
lines(sex.ratio[sex.ratio$replicate=="rep_2_up",c("generation","sex.ratio")],type="b",lty=2,lwd=2,cex=2,pch=22)


dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Sex_ratio_basolo/Sex_ratio_basolo.pdf")



