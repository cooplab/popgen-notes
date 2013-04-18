fig.direct="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/Figs/"


traj<-function(p,w11,w12,w22,tgen=200,plot.it=TRUE,add.it=FALSE,col="red"){

#w11<-.1
#w12<-1
#w22<-.1
p.array<-p
for(i in 1:tgen){
wbar<- w11*p^2 +w12*2*p*(1-p) + w22 * (1-p)^2
margin<-(w11*p + w12*(1-p)) - (w12*p + w22*(1-p))
d_p<-p*(1-p)*(margin) /(wbar)

p<- p+d_p
p.array<-c(p.array,p)
}
if(!add.it) plot(p.array,xlab="generations",ylab="Frequency of allele 1",type="l",lwd=3,col=col,ylim=c(0,1),cex.lab=1.5,cex.axis=1.5)
if(add.it) lines(p.array,lwd=3,col=col)

#if(plot.it==FALSE) return(p.array)
}

traj(p=0.01,w11=1, w12=1, w22=1-.02,tgen=2000,col="red")
traj(p=0.01,w11=1, w12=1-.01, w22=1-.02,tgen=2000,add.it=TRUE,col="black")
traj(p=0.01,w11=1, w12=1-0.02, w22=1-.02,tgen=2000,add.it=TRUE,col="blue")
legend(x=1300,y=0.5,col=c("red","black","blue"),lty=1,lwd=2,legend=paste("h = ",c(0,0.5,1)),cex=1.5)
dev.copy2eps(file=paste(fig.direct,"simple_diploid_trajs.eps",sep=""))

