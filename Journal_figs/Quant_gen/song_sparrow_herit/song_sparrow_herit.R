layout(t(1:2))
par(mar=c(4.2,4.2,1,1))
lims<-c(-2.5,2.5)
sparrow<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/song_sparrow_herit/song_sparrow_herit.csv")
plot(sparrow$beak_depth_midpar,sparrow$beak_depth_offspring,
     pch=19,cex=1.4,xlim=lims,ylim=lims,asp=1,
     xlab="Mid-parent Beak Depth",ylab="Offspring Mean Beak Depth",cex.lab=1.4,cex.axis=1.2) 
lm.output<-lm(sparrow$beak_depth_offspring~sparrow$beak_depth_midpar)
legend(x="topleft",legend=paste("Slope=", format(lm.output$coefficients[2],dig=2)),cex=1.4,bty="n")
abline(lm.output,col="red",lwd=2)
abline(0,1,col="grey")
plot(sparrow$tarsus_midpar,sparrow$tarsus_offspring,
     pch=19,cex=1.4,xlim=lims,ylim=lims,asp=1, xlab="Mid-parent Tarsus Length",ylab="Offspring Mean Tarsus Length",cex.lab=1.4,cex.axis=1.2) 
lm.output<-lm(sparrow$tarsus_offspring~sparrow$tarsus_midpar)
legend(x="topleft",legend=paste("Slope=", format(lm.output$coefficients[2],dig=2)),cex=1.4,bty="n")

abline(lm.output,col="red",lwd=2)
abline(0,1,col="grey")

dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/song_sparrow_herit/song_sparrow_herit.pdf")


#https://sci-hub.tw/https://www.jstor.org/stable/2408296?seq=1
foster.sparrow<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/song_sparrow_herit/song_sparrow_herit_foster.csv")
layout(1)
plot(foster.sparrow$foster_beak_depth,foster.sparrow$family_beak_depth,
     pch=19,cex=1.4,asp=1, xlab="Mid-Foster Parent Beak Depth",ylab="Offspring Mean Beak Depth",cex.lab=1.4,cex.axis=1.2)
lm.output<-lm(foster.sparrow$family_beak_depth~foster.sparrow$foster_beak_depth)
legend(x="topleft",legend=paste("Slope=", format(lm.output$coefficients[2],dig=2)),cex=1.4,bty="n")
abline(lm.output,col="red",lwd=2)
abline(0,1,col="grey")
dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/song_sparrow_herit/song_sparrow_herit_foster.pdf")
