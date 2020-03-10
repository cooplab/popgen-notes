

#layout(1:2)

D0<-0.25
r<-c(0.01,0.1,0.5)

t<-0:100

plot(x=range(t),y=c(0,0.25),type="n",ylab="D",xlab="Generations (t)",cex.lab=1.4,cex.axis=1.2)

cols=brewer.pal(3,"Dark2")
sapply(1:length(r),function(i){
Dt<- D0 *(1-r[i])^t
lines(t,Dt,col=cols[i],lwd=3)
})

 legend(x="topright",lty=1,col=cols,legend=paste("r =",r),cex=1.4,lwd=3)
 
 dev.copy2pdf(file="figures/LD_decay/LD_decay_time.pdf")
 
 
 
 D0<-0.25
my.r<-0:50/100

plot(x=range(r),y=c(0,0.25),type="n",ylab="D",xlab="Recombination fraction (r)",cex.lab=1.4,cex.axis=1.2)

ts<-c(5,10,100)

cols=brewer.pal(3,"Dark2")
sapply(1:length(r),function(i){
Dt<- D0 *(1-my.r)^ts[i]
lines(my.r,Dt,col=cols[i],lwd=3)
})

 legend(x="topright",lty=1,col=cols,legend=paste("t =",ts),cex=1.4,lwd=3)
 
 
 
