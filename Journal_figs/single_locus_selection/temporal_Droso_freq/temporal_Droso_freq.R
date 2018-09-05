Droso.inv<-read.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/temporal_Droso_freq/Wright_Dobzhansky.csv")

layout(t(1:2))

inv_freq<-Droso.inv[,grep("freq", colnames(Droso.inv))]
inv_freq_range<-range(inv_freq,na.rm=TRUE)/100 + c(-0.05,0.05)

plot(Droso.inv$months_Andreas_Canyon,Droso.inv$Andreas_Canyon_freq/100,type="b",
pch=18,cex=2,ylim=inv_freq_range,axes=FALSE,xlab="Month",ylab="Frequency",cex.lab=1.5,main= "Standard Inversion allele",cex.main=1.5)
points(Droso.inv$months_Pinon_Flats,Droso.inv$Pinon_Flats_freq/100,type="b",pch=19,cex=2)
points(Droso.inv$months_Keen_Camp,Droso.inv$Keen_Camp/100,type="b",pch=17,cex=2)

axis(side=2)
months<-c("J","F","M","A","M","J","J","A","S","O","N","D")
axis(side=1,at=1:12,lab=months)


y_text<-c(inv_freq[3,1],inv_freq[1,2],inv_freq[1,3])

text(rep(3,3),y=(y_text+2)/100,c("Andreas Canyon","Pinon Flats","Keen Camp"))


Droso_ILR<-read.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/temporal_Droso_freq/Paaby_ILR_freqs.csv")

plot(Droso_ILR$freq,type="b",pch=19,cex=2,ylim=c(.15,.4),axes=FALSE,xlab="Month",ylab="Frequency",cex.lab=1.5,main= "Insulin-like Receptor allele",cex.main=1.5)
axis(side=2)
axis(side=1,at=1:6,lab=Droso_ILR$month)

dev.copy2pdf(file="Journal_figs/single_locus_selection/temporal_Droso_freq/temporal_Droso_freq.pdf")