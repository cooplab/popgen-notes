

library("sf")


two.loc.sims<-function(p,w.mat,r){
	stopifnot(sum(p)==1)
	p.array<-matrix(NA,nrow=n.gens,ncol=4)
	d.array<-NA
	p.array[1,]<-p
	colnames(p.array)<-names(p)
	for(gen in 2:n.gens){
		w.marg<-apply(w.mat,1,function(whap){whap*p}) 
		w.marg<-colSums(w.marg)
		D<-p["AB"] - (p["AB"]+p["Ab"])*(p["AB"]+p["aB"])
		d.array<-c(d.array,D)
		D.vec<- c(-D,+D,+D,-D)
		wbar<-sum(p*w.marg)
		p.new<- (p*w.marg + r*D.vec*w.mat["AB","ab"])/wbar
		p.array[gen,]<-p.new
		p<-p.new
	}
	return(cbind(p.array,d.array))
}

# https://stackoverflow.com/questions/52522872/r-sf-package-centroid-within-polygon
stack.freqs.plot<-function(p.out,my.title=""){
	stacked.freqs<-t(apply(p.out[,c("Ab","AB","ab","aB")],1,cumsum))
	plot(stacked.freqs[,"Ab"],ylim=c(0,1),type="l",xlab="Generations",ylab="Frequencies",cex.lab=1.4,cex.axis=1.2,main=my.title,cex.main=1.4)
	my.x<-1:n.gens
	x.polygon<-c(my.x,rev(my.x))
#	recover()
	polygon(x=x.polygon,c(stacked.freqs[,"Ab"],rep(0,n.gens)),col="blue")
 	pol <-st_polygon(list(cbind(c(x.polygon,1), c(stacked.freqs[,"Ab"],rep(0,n.gens),stacked.freqs[,"Ab"][1]))))
 	centroid<-st_centroid(pol)
	text(centroid[1],centroid[2],col="white","Ab",cex=1.4) 

	polygon(x=x.polygon,c(stacked.freqs[,"AB"],rev(stacked.freqs[,"Ab"])),col="purple")
 	pol <-st_polygon(list(cbind(c(x.polygon,1), c(stacked.freqs[,"AB"],rev(stacked.freqs[,"Ab"]),stacked.freqs[,"AB"][1]))))
 	centroid<-st_centroid(pol)
	text(centroid[1],centroid[2],col="white","AB",cex=1.4) 
	
	polygon(x=x.polygon,c(stacked.freqs[,"AB"],rev(stacked.freqs[,"aB"])),col="white")
	pol <-st_polygon(list(cbind(c(x.polygon,1) ,c(stacked.freqs[,"AB"],rev(stacked.freqs[,"aB"]),stacked.freqs[,"AB"][1]))))
 	centroid<-st_centroid(pol)
	text(centroid[1]/3,centroid[2],col="black","ab",cex=1.4) 
		
	polygon(x=x.polygon,c(stacked.freqs[,"aB"],rev(stacked.freqs[,"ab"])),col="red")
	pol <-st_polygon(list(cbind(c(x.polygon,1) ,c(stacked.freqs[,"aB"],rev(stacked.freqs[,"ab"]),stacked.freqs[,"aB"][1]))))
 	centroid<-st_centroid(pol)
	if(any(stacked.freqs[,"aB"]<0.98)) text(centroid[1],centroid[2],col="black","aB",cex=1.4) 

}


# stack.freqs.plot<-function(p.out,my.order=c("Ab","AB","aB","ab")){
	# cols<-c("white","blue","red","purple")
	# names(cols)<-c("ab","Ab","aB","purple")
	# stacked.freqs<-t(apply(p.out[,c("Ab","AB","ab","aB")],1,cumsum))
	# plot(stacked.freqs[,"Ab"],ylim=c(0,1),type="n",xlab="Generations",ylab="Frequencies",cex.lab=1.2)
	# my.x<-1:n.gens
	# x.polygon<-c(my.x,rev(my.x))
	# polygon(x=x.polygon,c(stacked.freqs[,my.order[1]],rep(0,n.gens)),col=cols[my.order[1]])
	# polygon(x=x.polygon,c(stacked.freqs[,my.order[2]],rev(stacked.freqs[,my.order[1]])),,col=cols[my.order[2]])
	# polygon(x=x.polygon,c(stacked.freqs[,my.order[2]],rev(stacked.freqs[,my.order[3]])),,col=cols[my.order[3]])
	# polygon(x=x.polygon,c(stacked.freqs[,my.order[3]],rev(stacked.freqs[,my.order[4]])),,col=cols[my.order[4]])
# }


###Neutral Hitchhiking
n.gens<-500
p<-  c(0.001,0.099,0,0.9); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(1,0.95,1,0.95);names(w.add)<-names(p)
w.mat<-outer(w.add,w.add,FUN="+")
layout(t(1:3))
par(mar=c(4,4,3,1))
p.out<-two.loc.sims(p,w.mat,r=0.0005); stack.freqs.plot(p.out,my.title="r=0.0005")
#plot(p.out[,"AB"]+p.out[,"Ab"],type="l",ylim=c(0,1))#lines(p.out[,"AB"]+p.out[,"aB"],col="red")
p.out<-two.loc.sims(p,w.mat,r=0.005); stack.freqs.plot(p.out,my.title="r=0.005")
p.out<-two.loc.sims(p,w.mat,r=0.05);stack.freqs.plot(p.out,my.title="r=0.05")
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/selection_recom_interaction/Neutral_Hitchhiking.pdf")

###Deleterious allele Hitchhiking
par(mar=c(4,4.1,3,1))
n.gens<-1000
p<-  c(0.001,0.099,0,0.9); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(0.97,0.93,1,0.95);names(w.add)<-names(p)
w.mat<-outer(w.add,w.add,FUN="+")

layout(t(1:3))
 p.out<-two.loc.sims(p,w.mat,r=0.00);stack.freqs.plot(p.out,my.title="r=0.0");
 p.out<-two.loc.sims(p,w.mat,r=0.000005);stack.freqs.plot(p.out,my.title="r=0.000001");
 p.out<-two.loc.sims(p,w.mat,r=0.0002);stack.freqs.plot(p.out,my.title="r=0.0005");

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Deleterious_Hitchhiking.pdf")

##interference
layout(t(1:2))
n.gens<-1000
p<-  c(0.0,0.01,0.01,0.98); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(1.14,1.08,1.06,1);names(w.add)<-names(p)
w.mat<-outer(w.add,w.add,FUN="+")
p.out<-two.loc.sims(p,w.mat,r=0.00)
stack.freqs.plot(p.out)


p<-  c(0.0,0.01,0.01,0.98); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(1.1,1.06,1.055,1);names(w.add)<-names(p)
w.mat<-outer(w.add,w.add,FUN="+")
p.out<-two.loc.sims(p,w.mat,r=0.001)
stack.freqs.plot(p.out)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Interference.pdf")

###synergistic alleles & recom
n.gens<-200
layout(t(1:3))
p<-  c(0.3,0.0,0.0,0.7); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(1.05,0.1,0.1,1.0);names(w.add)<-names(p);w.mat<-outer(w.add,w.add,FUN="+")
p.out<-two.loc.sims(p,w.mat,r=0.00)
stack.freqs.plot(p.out)

p<-  c(0.3,0.0,0.0,0.7); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(1.05,0.1,0.1,1.0);names(w.add)<-names(p);w.mat<-outer(w.add,w.add,FUN="+")
p.out<-two.loc.sims(p,w.mat,r=0.05)
stack.freqs.plot(p.out)


p<-  c(0.3,0.0,0.0,0.7); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(1.05,0.1,0.1,1.0);names(w.add)<-names(p);w.mat<-outer(w.add,w.add,FUN="+")
p.out<-two.loc.sims(p,w.mat,r=0.1)
stack.freqs.plot(p.out)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/synergistic_alleles_recom.pdf")


my.fit.mat<-function(p){
p_ab_ab<-outer(p,p)["ab","ab"]

w_ab_ab<-dbeta(x=p_ab_ab,1,2)
w_A_B<-dbeta(x=1-p_ab_ab,1,2)

w.mat<-matrix(data=1,nrow=4,ncol=4,dimnames=list(names(p),names(p)))

w.mat[grepl("A",rownames(w.mat)), grepl("B",colnames(w.mat)) ] <- w_A_B
w.mat[grepl("B",rownames(w.mat)), grepl("A",colnames(w.mat)) ] <- w_A_B

w.mat["ab","ab"]<-w_ab_ab
w.mat
}


##################Two locus sims to show affect of recom in -ve freq dependent selection models to demo. supergenes

p<-  c(0.001,0.099,0,0.9); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(0.97,0.93,1,0.95);names(w.add)<-names(p)
w.mat<-outer(w.add,w.add,FUN="+")


my.fit.mat<-function(p){
p_ab_ab<-outer(p,p)["ab","ab"]

w_ab_ab<-dbeta(x=p_ab_ab,1,1.1)+0.3
w_A_B<-dbeta(x=1-p_ab_ab,1,1.1)+0.3

w.mat<-matrix(data=0.3,nrow=4,ncol=4,dimnames=list(names(p),names(p)))

w.mat["AB",]<-w_A_B
w.mat[,"AB"]<-w_A_B
w.mat[grepl("A",rownames(w.mat)), grepl("B",colnames(w.mat)) ] <- w_A_B
w.mat[grepl("B",rownames(w.mat)), grepl("A",colnames(w.mat)) ] <- w_A_B

w.mat["ab","ab"]<-w_ab_ab
w.mat
}



two.supergene.sims<-function(p,r){
	stopifnot(sum(p)==1)
	p.array<-matrix(NA,nrow=n.gens,ncol=4)
	d.array<-NA
	p.array[1,]<-p
	colnames(p.array)<-names(p)
	for(gen in 2:n.gens){
		w.mat<-my.fit.mat(p)
		w.marg<-apply(w.mat,1,function(whap){whap*p}) 
		w.marg<-colSums(w.marg)
		D<-p["AB"] - (p["AB"]+p["Ab"])*(p["AB"]+p["aB"])
		d.array<-c(d.array,D)
		D.vec<- c(-D,+D,+D,-D)
		wbar<-sum(p*w.marg)
		p.new<- (p*w.marg + r*D.vec*w.mat["AB","ab"])/wbar
		p.array[gen,]<-p.new
		p<-p.new
	}
	return(cbind(p.array,d.array))
} 

n.gens<-100
layout((1:2))
par(mar=c(4,4,0,0))
p<-  c(0.25,0.25,0.25,0.25); names(p)<-c("AB","Ab","aB","ab")
p.out<-two.supergene.sims(p,r=0.00); stack.freqs.plot(p.out); sum(p.out[90,1:3])
p<-  c(0.25,0.25,0.25,0.25); names(p)<-c("AB","Ab","aB","ab")
p.out<-two.supergene.sims(p,r=0.4); stack.freqs.plot(p.out); sum(p.out[90,1:3])

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/supergene_recom.pdf")