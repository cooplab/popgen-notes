
N.vec<-c(rep(10,5),rep(3,2),rep(10,5))
num.gens<-length(N.vec)
offset<-0.1
plot(c(1,num.gens),c(1,max(N.vec))+c(-offset,offset),type="n",axes=FALSE,xlab="",ylab="")
mtext(side=1,line=1,"Generations")
 my.cols<-sample(rainbow(2*N))
 
N <-N.vec[1]
	points(rep(1,N),1:N+offset, pch=19,cex=1,col=my.cols[(1:N)*2])
 points(rep(1,N),1:N-offset, pch=19,cex=1,col=my.cols[(1:N)*2-1])

for(i in 1:num.gens){
	
N.new<-N.vec[i+1]
N.old<-N.vec[i]

 new.cols<-rep("black",2*N.new)

 for(ind in 1:N.new){
		par<-sample(1:N.old,2,replace=FALSE)

		which.allele<-sample(c(-1,1),1)
		lines(c(i,i+1), c(par[1]+which.allele*offset,ind-offset),col="grey",lwd=0.5)
		new.cols[2*ind-1]<- my.cols[2*par[1] +ifelse(which.allele==1,0,-1)]

		which.allele<-sample(c(-1,1),1)
		lines(c(i,i+1), c(par[2]+which.allele*offset,ind+offset),col="grey",lwd=0.5)
		new.cols[2*ind]<- my.cols[2*par[2] +ifelse(which.allele==1,0,-1)]

	}
	
 
 points(rep(i,N.old),1:N.old+offset, pch=19,cex=1,col=my.cols[(1:N)*2])
 points(rep(i,N.old),1:N.old-offset, pch=19,cex=1,col=my.cols[(1:N)*2-1])
 my.cols<-new.cols

}
