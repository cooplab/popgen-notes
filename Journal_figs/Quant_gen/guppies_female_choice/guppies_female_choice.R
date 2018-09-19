##data from Houde http://rspb.royalsocietypublishing.org/content/256/1346/125

guppies.m<-read.csv(file="Journal_figs/Quant_gen/guppies_female_choice/guppies_female_choice.csv")
guppies.m$up_avg<-rowMeans(guppies.m[,grep("up",colnames(guppies.m))],na.rm=TRUE)
guppies.m$down_avg<-rowMeans(guppies.m[,grep("down",colnames(guppies.m))],na.rm=TRUE)

guppies.f<-read.csv(file="Journal_figs/Quant_gen/guppies_female_choice/guppies_female_choice.csv")
guppies.f$up_avg<-rowMeans(guppies.f[,grep("up",colnames(guppies.f))],na.rm=TRUE)
guppies.f$down_avg<-rowMeans(guppies.f[,grep("down",colnames(guppies.f))],na.rm=TRUE)

layout(t(1:2))
plot(guppies.m$up_avg[1:3],type="b",pch=24,cex=1.5,bg="black",xlab="Generation",ylab="Mean Orange Area",cex.lab=1.4,ylim=range(c(guppies.m$up_avg,guppies.m$down_avg))
 points(guppies.m$down_avg[1:3],type="b",pch=25,cex=1.5,bg="black")
legend(x="topleft",pch=c(24,25),legend=c("Up Selection","Down Selection"),cex=1.5)

plot(guppies.f$up_avg[1:3],type="b",ylim=c(0.8,2.3),pch=24,cex=1.5,bg="black",xlab="Generation",ylab="Mean Female Orange Preference",cex.lab=1.2)
 points(guppies.f$down_avg[1:3],type="b",pch=25,cex=1.5,bg="black")
 
 dev.copy2pdf(file="Journal_figs/Quant_gen/guppies_female_choice/guppies_female_choice.pdf")