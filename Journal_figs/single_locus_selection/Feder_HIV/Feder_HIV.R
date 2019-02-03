
par(mar = c(5,5,2,5))
Feder_hiv<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Feder_HIV/Feder_HIV.csv")
Feder_hiv$week2_virial_load[c(1:3)]<-NA
Feder_hiv$virial_load_2[c(1:3)]<-NA
plot(Feder_hiv$week2_virial_load,Feder_hiv$virial_load_2,log="y",type="b",ylab="Plasma numbers of SHIV (copies per ml)",xlab="Weeks since infection",cex.axis=1.2,cex.lab=1.4)
par(new = T); plot(Feder_hiv$week2,Feder_hiv$freq2,type="b",axes=FALSE,xlim=range(Feder_hiv$week2_virial_load,na.rm=TRUE),col="red",xlab="",ylab="")
axis(4)
mtext(side=4,"Frequency of drug resistant allele",col="red",cex=1.4,line=3)
abline(v=12,col="grey")
text(x=11.5,y=20,"Start of drug treatment",srt=90,cex=1.2)

dev.copy2pdf(file="Popgen_teaching_Notes/Journal_figs/single_locus_selection/Feder_HIV/Feder_HIV.pdf")