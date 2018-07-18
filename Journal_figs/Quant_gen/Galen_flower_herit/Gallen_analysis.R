
Galen<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Galen_flower_herit/Galen_flower_par_offspring_data.csv",head=FALSE)

plot(Galen$V1,Galen$V2,pch=19,xlab="Maternal corolla flare",ylab="Offspring corolla flare",xlim=c(7,18),ylim=c(7,18),cex.lab=1.5)
abline(lm(Galen$V2~Galen$V1))
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Galen_flower_herit/Galen_corolla_flare.pdf")

#library(imager)
#image <- load.image("~/Dropbox/Courses/Popgen_teaching_Notes/illustration_images/Quant_gen/Polemonium_viscosum_Galen/Polemonium_viscosum.jpg")
#plot(image)
