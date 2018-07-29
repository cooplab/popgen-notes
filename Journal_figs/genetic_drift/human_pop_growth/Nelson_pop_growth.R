Nelson_pop_growth<-read.csv(file="Journal_figs/genetic_drift/human_pop_growth/Nelson_pop_growth.csv")

Nelson_pop_growth$x<-rowMeans(cbind(Nelson_pop_growth$syn.x, Nelson_pop_growth$nonsyn.x))

plot(Nelson_pop_growth$x,Nelson_pop_growth$nonsyn.y,log="xy",axes=FALSE,cex.lab=1.4,
xlab="Minor allele count",ylab="SNP count per kb")
lines(Nelson_pop_growth$pi_syn.x,Nelson_pop_growth$pi_syn.y,col="red",lwd=2)

points(Nelson_pop_growth$x,Nelson_pop_growth$nonsyn.y,pch=15,cex=1.5,type="b")
points(Nelson_pop_growth$x,Nelson_pop_growth$syn.y,pch=19,type="b",cex=1.5)

legend(x="topright",lty=1,pch=c(19,15),legend=c("Synonymous","Non-Synonymous"),cex=1.5)
axis(1)
axis(2,at=1/c(10^(1:5),0.1))
text(Nelson_pop_growth$pi_syn.x[1]*1.2,Nelson_pop_growth$pi_syn.y[1]*1.5,
expression(theta[pi]),cex=1.5)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/human_pop_growth/Nelson_pop_growth.pdf")