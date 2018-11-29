library("latex2exp")

dhfr<-read.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection//malaria_sweep/dhfr_sweep.csv")
#plot(dhfr$dist_dhfr_kb,dhfr$het_non_sweep,type="b",pch=19,ylim=c(0,1))
#points(dhfr$dist_dhfr_kb,dhfr$het_sweep,type="b")

#plot(dhfr$dist_dhfr_kb,dhfr$het_sweep/dhfr$het_non_sweep,type="b")

par(mar=c(5,5,1,1))
plot(dhfr$dist_dhfr_kb,dhfr$het_sweep,type="b",pch=19,ylim=c(0,1),xlim=c(-200,200),cex.axis=1.2,cex.lab=1.4,xlab="Distance from dhfr (kb)",ylab=TeX("Heterozygosity ($H_e$)"))

regional.het<-mean(dhfr$het_non_sweep)
abline(h=regional.het,lty=3)

dhfr$genetic.dist<-abs(dhfr$dist_dhfr_kb/(1500))




nls(het_sweep~ regional.het*(1-exp(-genetic.dist*tau)),data=dhfr,start=list(tau=10))

tau<-37.95 ##from nls fit
my.dists<-seq(min(dhfr$dist_dhfr_kb),max(dhfr$dist_dhfr_kb),length=1000)
my.genetic.dists<- abs(my.dists) /1500
my.pred.pi<-regional.het*(1-exp(-my.genetic.dists*tau))

lines(my.dists,my.pred.pi,lty=2)
legend(x="bottomright",lty=1:3,pch=c(19,NA,NA),legend=c("Observed","Fitted sweep","background levels"),cex=1.2)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/malaria_sweep/dhfr_sweep.pdf")