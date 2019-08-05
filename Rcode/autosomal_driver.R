
w22<-1
w12<-0.95
w11<-0.1

p.init<-1/10  #1/1000

mean.fit<-function(p){ 
	 p^2*w11 + w12*2*p*(1-p) + w22*(1-p)^2 
}

delta_p<-function(p){ 
	w1<- w11*p + w12*(1-p)
	w2<- w12*p + w22*(1-p)
	 wbar<-mean.fit(p) 
	 (p^2*w11 + w12*2*d*p*(1-p))/wbar  - p
}

d<-0.7


layout(1)
p<-p.init
my.p.traj<-p

for(i in 1:75){
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}
plot(my.p.traj,ylim=c(0,1),type="l",xlab="Generations",ylab="p",cex.axis=1.2,cex.lab=1.4,lwd=2)

abline(h=(2*d*w12-1)/(2*w12-w11-1),lty=2)

p<-p.init
my.p.traj<-p
d<-0.9
for(i in 1:75){
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}

lines(my.p.traj,ylim=c(0,1),type="l",xlab="Generations",ylab="p",cex.axis=1.2,cex.lab=1.4,lwd=2,col="red")
abline(h=(2*d*w12-1)/(2*w12-w11-1),lty=2,col="red")


p<-p.init
my.p.traj<-p
d<-0.5
for(i in 1:75){
	p<-p+delta_p(p)
	my.p.traj<-c(my.p.traj,p)
}

lines(my.p.traj,ylim=c(0,1),type="l",xlab="Generations",ylab="p",cex.axis=1.2,cex.lab=1.4,lwd=2,col="blue")
abline(h=(2*d*w12-1)/(2*w12-w11-1),lty=2,col="blue")


 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/autosomal_driver.pdf")