
#https://www.journals.uchicago.edu/doi/pdfplus/10.1086/303324
#https://www.nature.com/articles/nrg2664#ref102
silene<-read.csv(file="Journal_figs/single_locus_selection/Silene_inbreeding_Richards/Silene_inbreeding_depression.csv")


bins<-seq(-.01,0.5,length=5)

silene$bin<-cut(silene$F, bins)

wobbly_F_means<-tapply(silene$F,silene$bin,mean)

F_means<-c(0,0.125, 0.250, 0.375 )
names(F_means)<-names(wobbly_F_means)

silene$corrected_F<-F_means[silene$bin]

par(mar=c(4.1,4.5,1,1))
plot(jitter(silene$corrected_F,amount=0.005),silene$germ,pch=19,ylab="Germination Rate",xlab="Inbreeding coeff",cex=1.5,cex.lab=2,cex.axis=1.5)

 text(x=F_means, y=c(0.7,0.25,0.75,0.5), c("Outbred","Half-sib","Full-sib","Two gens. sib"),srt=90,cex=2,col=adjustcolor("red",0.8)) # "darkgrey") #adjustcolor("black",.6))
 
 dev.copy2pdf(file="Journal_figs/single_locus_selection/Silene_inbreeding_Richards/Silene_inbreeding_depression.pdf")