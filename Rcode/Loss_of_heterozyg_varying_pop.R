
#install.packages("plotrix")
#library("plotrix")
N.vec<- rep(5,30)   #c(rep(10,5),rep(3,2),rep(10,5),rep(3,2),rep(10,5))
const.RS<-TRUE
mutation<- TRUE  #FALSE
mut.rate<-  .1

for.class<-FALSE

if(for.class){
	line.lwd<-1
	line.col<-"black"
	mut.line.lwd<-1
	mut.line.col<-"black"

}else{

	line.lwd<-0.5
	line.col<-"grey"
	mut.line.lwd<-1
	mut.line.col<-"grey"
	
	}


#N.vec<-rep(5,20)
#N.vec<-c(rep(10,5),rep(3,2),rep(10,5))
num.gens<-length(N.vec)-1
offset<-0.1
plot(c(1,num.gens),c(1,max(N.vec))+c(-offset,offset),type="n",axes=FALSE,xlab="",ylab="")
mtext(side=1,line=1,"Generations")
 
N <-N.vec[1]
 my.cols<-rep("black",2*N)  #sample(rainbow(2*N))

	points(rep(1,N),1:N+offset, pch=19,cex=1.3,col=my.cols[(1:N)*2])
 points(rep(1,N),1:N-offset, pch=19,cex=1.3,col=my.cols[(1:N)*2-1])

for(i in 1:num.gens){
		
	N.new<-N.vec[i+1]
	N.old<-N.vec[i]
	
	 new.cols<-rep("black",2*N.new)
	
	if(const.RS){ 
		repro.success<-rep(1/N.old,N.old)
	}else{
		repro.success<-sample(c(rep(0.5/(N.old),N.old-2),c(0.25,0.25)),replace=FALSE)
		}
	
	
	
	 for(ind in 1:N.new){
			par<-sample(1:N.old,2,replace=FALSE,prob=repro.success)
	
			which.allele.1<-sample(c(-1,1),1)
			if(i != num.gens){ lines(c(i,i+1), c(par[1]+which.allele.1*offset,ind-offset),col=line.col,lwd=line.lwd)}
			new.cols[2*ind-1]<- my.cols[2*par[1] +ifelse(which.allele.1==1,0,-1)]
	
			which.allele.2<-sample(c(-1,1),1)
			if(i != num.gens){ lines(c(i,i+1), c(par[2]+which.allele.2*offset,ind+offset),col=line.col,lwd=line.lwd)}
			new.cols[2*ind]<- my.cols[2*par[2] +ifelse(which.allele.2==1,0,-1)]
	
			if(mutation){
				if(runif(1)<mut.rate){ 
						new.cols[2*ind-1]<- sample(rainbow(4*N),1)
						if(i != num.gens){ lines(c(i,i+1), c(par[1]+which.allele.1*offset,ind-offset),col=mut.line.col,lwd=mut.line.lwd)}
	
					}
				if(runif(1)<mut.rate){ 
					new.cols[2*ind]<- sample(rainbow(4*N),1)
					if(i != num.gens){ lines(c(i,i+1), c(par[2]+which.allele.2*offset,ind+offset),col=mut.line.col,lwd=mut.line.lwd)}
			} 
			
		}
	}	
	 
	 points(rep(i,N.old),1:N.old+offset, pch=19,cex=1.3,col=my.cols[(1:N)*2])
	 points(rep(i,N.old),1:N.old-offset, pch=19,cex=1.3,col=my.cols[(1:N)*2-1])
	 my.cols<-new.cols
	if(!const.RS) sapply(which(repro.success>1/N.old), function(ind){ draw.circle(x=i,y=ind,radius=0.3,nv=100,border=NULL,col=NA,lty=1,lwd=1)})
}
