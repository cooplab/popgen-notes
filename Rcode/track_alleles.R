N<-10
offset<-0.2
###Track pairs
pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/pairwise_coalescent.pdf")
col.allele<-  c("red","blue")   #c("red","blue","purple") #
num.tracked<-2

###OR

###Track trio of lineages
pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/3_tip_coalescent.pdf")
col.allele<-  c("red","blue","purple") #c("red","blue")   #
num.tracked<-3

num.gens<-20
n.iter<-20
for(iter in 1:n.iter){
plot(c(1,num.gens),c(1,N),type="n",axes=FALSE,xlab="",ylab="")
mtext(side=1,line=1,"Generations")

track.this.allele<-vector("list", 2*N)

track.this.allele[sample(1:(2*N),num.tracked)]<-1:num.tracked

track.this.allele.next.gen<-vector("list", 2*N)

for(i in num.gens:1){
	
for(ind in 1:N){
	
		par<-sample(1:N,2,replace=FALSE)
		which.allele<-sample(c(-1,1),1)
		lines(c(i,i-1), c(ind-offset,par[1]+which.allele*offset),col="light grey",lwd=0.5)
		if(!is.null(track.this.allele[[2*ind-1]])){
			this.one<-2*par[1] +ifelse(which.allele==1,0,-1); 
			track.this.allele.next.gen[[this.one]]  <- c(track.this.allele.next.gen[[this.one]],track.this.allele[[2*ind-1]])
			}
			
		which.allele<-sample(c(-1,1),1)
		lines(c(i,i-1), c(ind+offset,par[2]+which.allele*offset),col="light grey",lwd=0.5)
		if(!is.null(track.this.allele[[2*ind]])){ 
					this.one<-2*par[2] +ifelse(which.allele==1,0,-1); 
			track.this.allele.next.gen[[ this.one]]  <- c(track.this.allele.next.gen[[this.one]],track.this.allele[[2*ind]])
			}
#		recover()
	}
	for(this.allele in 1:num.tracked){ 
		daughter<-which(sapply(track.this.allele,function(allele){any(allele==this.allele)}))
		parent<-which(sapply(track.this.allele.next.gen,function(allele){any(allele==this.allele)}))
		lines(c(i,i-1), c(ceiling(daughter/2)+offset* ifelse(daughter %% 2,-1,1) ,ceiling(parent/2) + offset*ifelse(parent %% 2,-1,1) ),col=col.allele[this.allele],lwd=2)
		}
		
	points(rep(i,N),1:N+offset, pch=19,cex=1)
	points(rep(i,N),1:N-offset, pch=19,cex=1)
	 track.this.allele<-track.this.allele.next.gen
	 track.this.allele.next.gen<-vector("list", 2*N)
	}
	}
dev.off()	

###coal vs geo

layout(1:2); par(mar=c(0.5,0.5,0.5,0.5))
n.iter<-1

##run above here with n.iter=1 n=30, N=20 


plot(-(1:30),c(NA,dgeom(1:29,1/20)),pch=19,cex=2,xlab="generations back to coalescence",ylab="Probability",ylim=c(0.0,0.05)); lines(-(1:30),c(NA,exp(-(1:29)/20)/20),col="red",lwd=3)
abline(v=-21,col="blue",lwd=3)

dev.copy2pdf(file="~/Downloads/Coal_vs_geometric.pdf")

