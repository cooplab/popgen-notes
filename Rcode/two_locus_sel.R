


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

stack.freqs.plot<-function(p.out,my.order){
	stacked.freqs<-t(apply(p.out[,c("Ab","AB","ab","aB")],1,cumsum))
	plot(stacked.freqs[,"Ab"],ylim=c(0,1),type="l",xlab="Generations",ylab="Frequencies",cex.lab=1.2)
	my.x<-1:n.gens
	x.polygon<-c(my.x,rev(my.x))
	polygon(x=x.polygon,c(stacked.freqs[,"Ab"],rep(0,n.gens)),col="blue")
	polygon(x=x.polygon,c(stacked.freqs[,"AB"],rev(stacked.freqs[,"Ab"])),col="purple")
	polygon(x=x.polygon,c(stacked.freqs[,"AB"],rev(stacked.freqs[,"aB"])),col="white")
	polygon(x=x.polygon,c(stacked.freqs[,"aB"],rev(stacked.freqs[,"ab"])),col="red")
}


stack.freqs.plot<-function(p.out,my.order=c("Ab","AB","aB","ab")){
	cols<-c("white","blue","red","purple")
	names(cols)<-c("ab","Ab","aB","purple")
	stacked.freqs<-t(apply(p.out[,c("Ab","AB","ab","aB")],1,cumsum))
	plot(stacked.freqs[,"Ab"],ylim=c(0,1),type="n",xlab="Generations",ylab="Frequencies",cex.lab=1.2)
	my.x<-1:n.gens
	x.polygon<-c(my.x,rev(my.x))
	polygon(x=x.polygon,c(stacked.freqs[,my.order[1]],rep(0,n.gens)),col=cols[my.order[1]])
	polygon(x=x.polygon,c(stacked.freqs[,my.order[2]],rev(stacked.freqs[,my.order[1]])),,col=cols[my.order[2]])
	polygon(x=x.polygon,c(stacked.freqs[,my.order[2]],rev(stacked.freqs[,my.order[3]])),,col=cols[my.order[3]])
	polygon(x=x.polygon,c(stacked.freqs[,my.order[3]],rev(stacked.freqs[,my.order[4]])),,col=cols[my.order[4]])
}


###Neutral Hitchhiking
n.gens<-500
p<-  c(0.001,0.099,0,0.9); names(p)<-c("AB","Ab","aB","ab")
#p<-  c(0.4,.1,.1,0.4); names(p)<-c("AB","Ab","aB","ab")
#w.add<-c(1,1,1,1);names(w.add)<-names(p)

w.add<-c(1,0.95,1,0.95);names(w.add)<-names(p)
w.mat<-outer(w.add,w.add,FUN="+")

layout(matrix(1:4,nrow=2,byrow=TRUE))
par(mar=c(2,2,1,1))
p.out<-two.loc.sims(p,w.mat,r=0.000)
stack.freqs.plot(p.out)
#plot(p.out[,"AB"]+p.out[,"Ab"],type="l",ylim=c(0,1))
#lines(p.out[,"AB"]+p.out[,"aB"],col="red")

p.out<-two.loc.sims(p,w.mat,r=0.0005)
stack.freqs.plot(p.out)
#plot(p.out[,"AB"]+p.out[,"Ab"],type="l",ylim=c(0,1))#lines(p.out[,"AB"]+p.out[,"aB"],col="red")

p.out<-two.loc.sims(p,w.mat,r=0.005)
stack.freqs.plot(p.out)
#plot(p.out[,"AB"]+p.out[,"Ab"],type="l",ylim=c(0,1)); lines(p.out[,"AB"]+p.out[,"aB"],col="red")

p.out<-two.loc.sims(p,w.mat,r=0.05)
stack.freqs.plot(p.out)
#plot(p.out[,"AB"]+p.out[,"Ab"],type="l",ylim=c(0,1));lines(p.out[,"AB"]+p.out[,"aB"],col="red")


###Deleterious allele Hitchhiking
n.gens<-750
p<-  c(0.001,0.099,0,0.9); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(0.97,0.93,1,0.95);names(w.add)<-names(p)
w.mat<-outer(w.add,w.add,FUN="+")

layout(t(1:3))
 p.out<-two.loc.sims(p,w.mat,r=0.00);stack.freqs.plot(p.out);
 p.out<-two.loc.sims(p,w.mat,r=0.00005);stack.freqs.plot(p.out);
 p.out<-two.loc.sims(p,w.mat,r=0.0002);stack.freqs.plot(p.out);

##interference
layout(t(1:2))
n.gens<-1000
p<-  c(0.0,0.01,0.01,0.98); names(p)<-c("AB","Ab","aB","ab")
w.add<-c(1.14,1.08,1.06,1);names(w.add)<-names(p)
w.mat<-outer(w.add,w.add,FUN="+")
p.out<-two.loc.sims(p,w.mat,r=0.00)
stack.freqs.plot(p.out)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Deleterious_Hitchhiking.pdf")

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