

orchid<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Elderflower_orchid/Elderflower_orchids.csv")

 layout(t(1:2))
plot(orchid$freq_yellow, orchid$male_success,col="black",ylim=range(orchid[,c("male_success","female_pollination_success")]),cex=1.4,pch=18,type="b",cex.lab=1.4,cex.axis=1.2,xlab="Frequency of Yellow morph",ylab="Relative reproductive success of Yellow",lty=2,lwd=2)
points(orchid$freq_yellow, orchid$female,col="black",cex=1.4,pch=19,type="b",lwd=2)
legend(x="topright",legend=c("Female (% pollinia deposited)", "Male (% pollinia removed)"),pch=c(19,18),cex=1.2)
abline(h=1,col="grey",lwd=2)



##Note that this model is not right as the freq. dependence comes from pollinator success, where her eit is modelled as viability. However, it serves to illustate the general principal

##yellow is thought to be recessive
##uses functions from 
fitness_model<-lm((orchid$male_success+orchid$female)/2~orchid$freq_yellow)$coeff


new_fitnesses<-function(p){ratio<-fitness_model[1] + fitness_model[2]*p
	wY<- (ratio/(2-ratio))
	wP<- 1
	c(wY,wP,wP)
}




mean.fit<-function(p){ 
	 p^2*w11 + w12*2*p*(1-p) + w22*(1-p)^2 
}

delta_p<-function(p){ 
	w1<- w11*p + w12*(1-p)
	w2<- w12*p + w22*(1-p)
	 wbar<-mean.fit(p) 
	p*(1-p)*(w1-w2)/wbar 
}

p<-1/100
my.p.traj<-p
for(i in 1:150){
	w<-new_fitnesses(p)
	w11<-w[1];w12<-w[2];w22<-w[3]
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}
 plot(my.p.traj,ylim=c(0,1),type="l",cex.axis=1.2,cex.lab=1.4,xlab="Generations",ylab="Yellow allele freq.",cex=1.4,lwd=2)

p<-99/100
my.p.traj<-p
for(i in 1:150){
	w<-new_fitnesses(p)
	w11<-w[1];w12<-w[2];w22<-w[3]
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}
 lines(my.p.traj,cex=1.4,lwd=2,lty=2)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Elderflower_orchid/Elderflower_orchids_fitness.pdf")
