
fig.direct="~/Dropbox/Courses/Popgen_teaching_Notes/figures/"

#############################
######## Parameters #########
#############################

	sigma = 1 		# standard dev of migration
	sel = c(0.1,0.01,.001)			# selection against non-resident allele #s = 0.01 #s = 0.001 #s = 0.0001
	delta = 1 		# grain of our derivative	

#############################
######## Functions ##########
#############################

	# standardize x (distance from cline center) by dividing by the characterist length
		GetXi =function(sigma,s,x){x*sqrt(s)/sigma} 

	# Get allele frequency at point x
		A = function(sigma,s,x){ xi=GetXi(sigma,s,x) 
			ifelse(xi>=0, (-1 + 3*tanh(xi/2 + atanh(sqrt(2/3)))^2)/2,	(3/2)*(1 - tanh(-xi/2 + atanh(sqrt(2/3)))^2)    ) }

x=-100:100
plot(x,1-A(sigma,sel[1],x),type="l",col="black",lwd=2,xlab="Position x, km",ylab="Frequency of allele 2, q(x)",cex.axis=1.5,cex.lab=1.5)
abline(v=0,col="lightgrey",lwd=2)
lines(x,1-A(sigma,sel[2],x),col="red",lwd=2)
lines(x,1-A(sigma,sel[3],x),col="blue",lwd=2)
legend("topright",legend=paste("s = ",sel),lty=1,lwd=2,col=c("black","blue","red"),cex=1.5)
text(c(-50,50),c(0.5,0.5),c("Allele 2 favoured","Allele 2 disfavoured"),cex=1)
dev.copy2eps(file=paste(fig.direct,"equilib_cline.eps",sep=""))

plot(x,1-A(sigma,sel[2],x),type="l",col="red",lwd=2,xlab="Position x, km",ylab="Frequency of allele 2, q(x)")
abline(a=0.5,b=-1.7*sigma/sqrt(sel[2]))
gradient.at.origin<-((1-A(sigma,sel[2],x))[102]- (1-A(sigma,sel[2],x))[99])/(x[102]-x[99])
abline(a=0.5,b=gradient.at.origin,lty=2,lwd=2)
abline(v= floor((1 -0.5)/ gradient.at.origin),lty=2,lwd=2)
 abline(v= ceiling((0 -0.5)/ gradient.at.origin),lty=2,lwd=2)
 dev.copy2pdf(file=paste(fig.direct,"equilib_width_cline.pdf",sep=""))