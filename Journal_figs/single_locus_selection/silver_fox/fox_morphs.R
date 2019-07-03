
#data from http://www.pnas.org/content/106/Supplement_1/9987
fox_morphs<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/silver_fox/fox_morphs.csv")
fox_morphs$years<-rowMeans(fox_morphs[,grep("years",colnames(fox_morphs))])
plot(fox_morphs$years,fox_morphs$silver_freq,ylim=c(0,100),pch=19,xlab="Year",ylab="Frequency",cex=1.3,cex.lab=1.4) #range(fox_morphs[,grep("freq",colnames(fox_morphs))])
points(fox_morphs$years,fox_morphs$cross_freq,pch=15,cex=1.3)
points(fox_morphs$years,fox_morphs$red_freq,pch=17,cex=1.3)
text(x=rep(fox_morphs$years[6],3),y=fox_morphs[6,grep("freq",colnames(fox_morphs))]+c(5,5,-5),c("silver (RR)","cross (Rr)","red (rr)"),cex=1.4)

#dev.copy2pdf(file="Journal_figs/single_locus_selection/silver_fox/fox_morph_freqs.pdf")

##fitting a simple recessive model to data
mean.fit<-function(p){ 
	 p^2*w11 + w12*2*p*(1-p) + w22*(1-p)^2 
}

delta_p<-function(p){ 
	w1<- w11*p + w12*(1-p)
	w2<- w12*p + w22*(1-p)
	 wbar<-mean.fit(p) 
	p*(1-p)*(w1-w2)/wbar 
}

sim.years<-seq(round(min(fox_morphs$years)),round(max(fox_morphs$years)),by=2)

w11<-0.90; w12<-1; w22<-1
p<-0.32; my.p.traj<-numeric()
for(i in 1:length(sim.years)){
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}
lines(sim.years,my.p.traj^2*100)
lines(sim.years,2*my.p.traj*(1-my.p.traj)*100)
lines(sim.years,(1-my.p.traj)^2*100)
