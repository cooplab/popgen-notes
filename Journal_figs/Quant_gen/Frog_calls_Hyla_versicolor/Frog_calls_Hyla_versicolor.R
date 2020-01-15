
##From   https://onlinelibrary.wiley.com/doi/pdf/10.1111/evo.12397

frogs<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Frog_calls_Hyla_versicolor/doi_10.5061_dryad.40sj6__v1/Hyla_versicolor_call_heritability.csv")

frog.dads<-frogs[frogs$id %in% 12:59,]

kids<-frogs[frogs$sire!=600 & frogs$dam!=600,]

layout(t(1:2))
plot(kids$CP,kids$PN,pch=19,
     col=adjustcolor("black",0.3),xlab="Call period (sec)", ylab="Pulse Number",cex=1.4,cex.lab=1.5,cex.axis=1.2,log="xy")

plot(tapply(kids$CP,kids$sire,mean),tapply(kids$PN,kids$sire,mean),log="xy",pch=19,
     col=adjustcolor("black",0.7),xlab="Son Call period (sec)", ylab="Son Pulse Number",cex=1.5,cex.lab=1.4,cex.axis=1.2)

dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Frog_calls_Hyla_versicolor/Frog_calls_sibling_means.pdf")

####Looking at covariance between sibs, not used

plot(range(kids$CP),range(kids$PN),type="n")
half.sibs<-numeric()
for(sire in 12:59){
  these.ids<-kids$id[kids$sire==sire]
  
  my.combos<-expand.grid(x=these.ids,y=these.ids)
  my.combos<-my.combos[my.combos$x != my.combos$y,]
  
  points(kids[as.character(my.combos$x),]$CP,kids[as.character(my.combos$y),]$PN,col=adjustcolor("black",0.3))
  half.sibs<-rbind(half.sibs,cbind(kids[as.character(my.combos$x),]$CP,kids[as.character(my.combos$y),]$PN))
}
cor.test(half.sibs[,1],half.sibs[,2])  ##not 1/2 sibs they're full sibs


frogs.extracted<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Frog_calls_Hyla_versicolor/Frog_calls_Hyla_versicolor.csv")
plot(frogs.extracted$Call.period..sec.,frogs.extracted$Call.duration..pulses.,cex=1.4,pch=19,col=adjustcolor("black",0.7),
     xlab="Call period (sec)", ylab="Call duration",cex.lab=1.4,cex.axis=1.2)

