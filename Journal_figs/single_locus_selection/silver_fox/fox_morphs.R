
#data redrawn from http://www.pnas.org/content/106/Supplement_1/9987
fox_morphs<-read.csv("Journal_figs/single_locus_selection/silver_fox/fox_morphs.csv")
fox_morphs$years<-rowMeans(fox_morphs[,grep("years",colnames(fox_morphs))])
plot(fox_morphs$years,fox_morphs$silver_freq,ylim=range(fox_morphs[,grep("freq",colnames(fox_morphs))]),pch=19,xlab="Year",ylab="Frequency",cex=1.3,cex.lab=1.4)
points(fox_morphs$years,fox_morphs$cross_freq,pch=15,cex=1.3)
points(fox_morphs$years,fox_morphs$red_freq,pch=17,cex=1.3)
text(x=rep(fox_morphs$years[6],3),y=fox_morphs[6,grep("freq",colnames(fox_morphs))]+c(5,5,-5),c("silver (RR)","cross (Rr)","red (rr)"),cex=1.4)

dev.copy2pdf(file="Journal_figs/single_locus_selection/silver_fox/fox_morph_freqs.pdf")