


red.deer<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/red_deer_selection_gradient/selection_grad_deer.csv")
red.deer[red.deer$LRS<0.5,"LRS"]<- 0. %%zeros from original log plot


  plot(red.deer,pch=19,cex.lab=1.4,cex.axis=1.3,main="",cex=1.2,xlab="Antler mass",ylab="Lifetime Reproductive Success")
abline(lm(red.deer$LRS~red.deer$antler),col="red",lwd=2)
dev.copy2pdf("Journal_figs/Quant_gen/red_deer_selection_gradient/selection_grad_deer.pdf")