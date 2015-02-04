png(file="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/Figs/Loss_of_het_2.png",width = 800, height = 400)
N<-5
num.gens<-15
offset<-0.1
plot(c(1,num.gens),c(1,N)+c(-offset,offset),type="n",axes=FALSE,xlab="",ylab="")
mtext(side=1,line=1,"Generations")
 my.cols<-sample(rainbow(2*N))
 
	points(rep(1,N),1:N+offset, pch=19,cex=1,col=my.cols[(1:N)*2])
 points(rep(1,N),1:N-offset, pch=19,cex=1,col=my.cols[(1:N)*2-1])

for(i in 1:num.gens){
 new.cols<-rep("black",2*N)

 for(ind in 1:N){
		par<-sample(1:N,2,replace=FALSE)

		which.allele<-sample(c(-1,1),1)
		lines(c(i,i+1), c(par[1]+which.allele*offset,ind-offset),col="grey",lwd=0.5)
		new.cols[2*ind-1]<- my.cols[2*par[1] +ifelse(which.allele==1,0,-1)]

		which.allele<-sample(c(-1,1),1)
		lines(c(i,i+1), c(par[2]+which.allele*offset,ind+offset),col="grey",lwd=0.5)
		new.cols[2*ind]<- my.cols[2*par[2] +ifelse(which.allele==1,0,-1)]

	}
	
 
 points(rep(i,N),1:N+offset, pch=19,cex=1,col=my.cols[(1:N)*2])
 points(rep(i,N),1:N-offset, pch=19,cex=1,col=my.cols[(1:N)*2-1])
 my.cols<-new.cols

}

dev.off()



png(file="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/Figs/Loss_of_het_2_alleles.png",width = 800, height = 400)
N<-5
num.gens<-15
offset<-0.1
plot(c(1,num.gens+1),c(1,N)+c(-offset,offset),type="n",axes=FALSE,xlab="",ylab="")
mtext(side=1,line=1,"Generations")
 my.cols<-  rep(c("blue","red"),N)  # sample(rep(c("blue","red"),N), size=2*N)
 
	points(rep(1,N),1:N+offset, pch=19,cex=1,col=my.cols[(1:N)*2])
 points(rep(1,N),1:N-offset, pch=19,cex=1,col=my.cols[(1:N)*2-1])
my.freq<-numeric()
for(i in 1:num.gens){
 new.cols<-rep("black",2*N)

 for(ind in 1:N){
		par<-sample(1:N,2,replace=FALSE)

		which.allele<-sample(c(-1,1),1)
		lines(c(i,i+1), c(par[1]+which.allele*offset,ind-offset),col="grey",lwd=0.5)
		new.cols[2*ind-1]<- my.cols[2*par[1] +ifelse(which.allele==1,0,-1)]

		which.allele<-sample(c(-1,1),1)
		lines(c(i,i+1), c(par[2]+which.allele*offset,ind+offset),col="grey",lwd=0.5)
		new.cols[2*ind]<- my.cols[2*par[2] +ifelse(which.allele==1,0,-1)]

	}
	
my.freq<- c(my.freq,sum(my.cols=="blue"))
 points(rep(i,N),1:N+offset, pch=19,cex=1,col=my.cols[(1:N)*2])
 points(rep(i,N),1:N-offset, pch=19,cex=1,col=my.cols[(1:N)*2-1])
 my.cols<-new.cols

}
 plot(my.freq/(2*15),ylim=c(0,1),type="b",col="blue",pch=19,xlab="generations",ylab="Blue frequency")

#dev.copy2pdf(file="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/Figs/Loss_of_het_2_alleles_3.pdf")



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
