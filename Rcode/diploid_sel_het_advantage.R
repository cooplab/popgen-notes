


p<-seq(0,1,length=100)

mean.fit<-function(p){ 
	 p^2*w11 + w12*2*p*(1-p) + w22*(1-p)^2 
}

delta_p<-function(p){ 
	w1<- w11*p + w12*(1-p)
	w2<- w12*p + w22*(1-p)
	 wbar<-mean.fit(p) 
	p*(1-p)*(w1-w2)/wbar 
}

w11<-0.9; w12<-1; w22<-0.85
layout(1:2)
p<-seq(0,1,length=100)
par(mar=c(3,3,0.5,0.5))
plot(p,delta_p(p),type="l",ylab="",lwd=2)
mtext(side=2,expression(Delta*p),cex=1.4,line=1.9)
abline(h=0,col="grey")
abline(v= (1-w22)/((1-w11)+(1-w22)),col="red",lwd=2)
plot(p,mean.fit(p),type="l",xlab="",ylab=,cex.axis=1.2,cex.lab=1.4,lwd=2)
mtext(side=2,expression(bar(w)),cex=1.4,line=1.9)
mtext(side=1,"p",cex=1.4,line=1.9)
abline(v= (1-w22)/((1-w11)+(1-w22)),col="red",lwd=2)

 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/het_advant_dp_wbar.pdf")

layout(1)
p<-1/1000
my.p.traj<-p

for(i in 1:150){
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}
plot(my.p.traj,ylim=c(0,1),type="l",xlab="Generations",ylab="p",cex.axis=1.2,cex.lab=1.4,lwd=2)

p<-999/1000
my.p.traj<-p

for(i in 1:150){
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}
lines(my.p.traj,ylim=c(0,1),lty=2,lwd=2)
abline(h= (1-w22)/((1-w11)+(1-w22)),col="red",lwd=2)


 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/het_advant_traj.pdf")
 
 
 
 
 layout(1)
p<-1/1000
my.p.traj<-p

for(i in 1:150){
	w<-new_fitnesses(p)
	w11<-w[1];w12<-w[2];w22<-w[3]
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}
plot(my.p.traj,ylim=c(0,1),type="l",xlab="Generations",ylab="p",cex.axis=1.2,cex.lab=1.4,lwd=2)
