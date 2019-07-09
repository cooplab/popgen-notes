

# Genotypes 			XX,XXr,XrXr,XY,XrY
# relative frequencies:	u,v,w,x,y
# relative viabilities:	1,a,b,1,c
#prop. males born to XrY fathers is (1-t)

##Start from a low freq and HWE
p<-1/100
u<-0.5*(1-p)^2
v<-0.5*2*p*(1-p)
w<-0.5*p^2	
x<-0.5*(1-p)
y<-0.5*p

a<-0.8  #1
b<-0.8
c<-0.8
t<-0.99 #0.5

freqs<-numeric()
for(gen in 1:250){
	u_next <- 0.5*x*(u + 0.5*v)
	v_next <- a*(0.5*x*(w+0.5*v) + t*y*(u+0.5*v))
	w_next <- b*t*y*(w+0.5*v)
	x_next <- (0.5*x+(1-t)*y)*(u+0.5*v)
	y_next <- c*(0.5*x + (1-t)*y)*(w+0.5*v)
	
	T_norm<-u_next +v_next +w_next +x_next +y_next
	
	freqs<-rbind(freqs,c(u_next/T_norm,v_next/T_norm,w_next/T_norm,x_next/T_norm,y_next/T_norm))
	u<-u_next/T_norm; v<-v_next/T_norm; w<-w_next/T_norm; x<-x_next/T_norm; y<-y_next/T_norm
}


plot((0.5*freqs[,2] + freqs[,3] + 0.5*freqs[,5])/(freqs[,1]+freqs[,2] + freqs[,3] +0.5*freqs[,4] +0.5*freqs[,5])
,type="l",lwd=2,xlab="Generations",ylab="Frequency",cex.lab=1.4,cex.axis=1.2)
lines(rowSums(freqs[,4:5]),lty=2,lwd=2)
legend(x="topleft",legend=c("Selfish X chr.","Males"),lty=c(1,2),lwd=2,cex=1.4)