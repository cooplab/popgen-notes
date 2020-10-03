##Data from Figure 1D

decode.inbreeding<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Decode_human_inbreeding/decode_inbreeding.csv")
par(mar=c(4.5,4.5,2,1))
layout(1)
plot(decode.inbreeding$child_lifespan,
     ylim=range(decode.inbreeding),
     pch=19,cex=2,cex.lab=1.5, xlab="Relatedness of parents (Cousins)",ylab="Std. offspring lifespan",axes=FALSE)

segments(x0=1:7,x1=1:7,y0=decode.inbreeding$lower_CI,y1=decode.inbreeding$upper_CI,lwd=3)

axis(side=2,cex.axis=1.4,at=c(-0.15,-.1,-0.05,0,0.05))
axis(side=1,at=1:7,label=c(">2nd","3rd","4th","5th","6th","7th","No"),cex.axis=2)