

hyena<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/hyena_mat_effects/hyena_fig1.csv")

layout(t(1:3))
par(mar=c(4,4.5,1,1))
tmp<-hyena[hyena$setup=="nonadopted_kid_genetic_mum",]
plot(tmp$mum,tmp$kid,xlim=c(-1,1),ylim=c(-1,1),pch=19,cex=2,
     xlab="Biological mother's rank",ylab="Child's rank",cex.lab=2,asp=1)
abline(0,1,col="grey",lwd=2)
abline(lm(tmp$kid~tmp$mum),col="red",lwd=2)
points(tmp$mum,tmp$kid,pch=19,cex=1.2)
text(x=-0.65,y=0.95,"Not adopted",cex=2)

tmp<-hyena[hyena$setup=="adopted_kid_genetic_mum",]
plot(tmp$mum,tmp$kid,xlim=c(-1,1),ylim=c(-1,1),pch=19,cex=2,
     xlab="Biological mother's rank",ylab="Child's rank",cex.lab=2,asp=1)
abline(0,1,col="grey",lwd=2)
abline(lm(tmp$kid~tmp$mum),col="red",lwd=2)
points(tmp$mum,tmp$kid,pch=19,cex=1.2)
text(x=-0.65,y=0.95,"Adopted",cex=2)

tmp<-hyena[hyena$setup=="adopted_kid_adopted_mum",]
plot(tmp$mum,tmp$kid,xlim=c(-1,1),ylim=c(-1,1),pch=19,cex=2,
     xlab="Adopted mother's rank",ylab="Child's rank",cex.lab=2,asp=1)
abline(0,1,col="grey",lwd=2)
abline(lm(tmp$kid~tmp$mum),col="red",lwd=2)
points(tmp$mum,tmp$kid,pch=19,cex=1.2)

text(x=-0.65,y=0.95,"Adopted",cex=2)

dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/hyena_mat_effects/hyena_mat_effects.pdf")
