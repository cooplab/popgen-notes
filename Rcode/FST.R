mu=1e-8;
Time<-seq(1,4e4,length=100);
N=1e4;
theta= 4*N*mu;
HB = 1-(1-mu)^(2*Time) + (1-mu)^(2*Time)*theta/(theta+1); 
HS =theta/(theta+1)
HT= (HB+HS)/2

plot(Time,1-HS/HT,type="l",lwd=2,xlab="Split time, generations",ylab="FST = 1-HS/HT")

lines(Time,Time/(4*N),col="red",lwd=2,lty=2)
