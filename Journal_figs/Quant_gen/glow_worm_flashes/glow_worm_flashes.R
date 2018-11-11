%http://rsbl.royalsocietypublishing.org/content/11/10/20150599
glow_worm<-read.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/glow_worm_flashes/glow_worm_flashes.csv")

plot(glow_worm,cex=1.5,pch=19,xlab=expression(paste("Lantern Size (", mm^2,")")),y="Number of Eggs",cex.axis=1.2,cex.lab=1.4)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/glow_worm_flashes/glow_worm_flashes.pdf")