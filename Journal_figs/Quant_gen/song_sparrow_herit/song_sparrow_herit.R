layout(t(1:2))
par(mar=c(4,4,1,1))
lims<-c(-2.5,2.5)
sparrow<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/song_sparrow_herit/song_sparrow_herit.csv")
plot(sparrow$beak_depth_midpar,sparrow$beak_depth_offspring,
     pch=19,cex=1.2,xlim=lims,ylim=lims,asp=1,
     xlab="Mid-parent Beak Depth",ylab="Offspring Mean Beak Depth",cex.lab=1.4,cex.axis=1.2) 
abline(lm(sparrow$beak_depth_offspring~sparrow$beak_depth_midpar),col="red",lwd=2)
abline(0,1,col="grey")
plot(sparrow$tarsus_midpar,sparrow$tarsus_offspring,
     pch=19,cex=1.2,xlim=lims,ylim=lims,asp=1, xlab="Mid-parent Tarsus Length",ylab="Offspring Mean Tarsus Length",cex.lab=1.4,cex.axis=1.2) 
abline(lm(sparrow$tarsus_offspring~sparrow$tarsus_midpar),col="red",lwd=2)
abline(0,1,col="grey")

dev.copy2pdf("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/song_sparrow_herit/song_sparrow_herit.pdf")