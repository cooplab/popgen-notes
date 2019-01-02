
#Downloaded data from http://datadryad.org/bitstream/handle/10255/dryad.63933/brandainEtAlPLoSGen2014.Robj?sequence=1

show(load("brandainEtAlPLoSGen2014.Robj"))



plot_mim<-function(my.comp,pull.this){
	nat.comp<-pairwise.focal.mim[["comps"]][[my.comp]]
	
	pi<-(nat.comp[,"diff.pos"]/nat.comp[,"all.pos"])
	pi[nat.comp[,"all.pos"]<100]<-NA
	
	
	
	my.region<-pi[pull.this]
	
	par(mar=c(4,4,1,4))  #few data points lost to 
	plot(my.region,pch=19,col=adjustcolor("black",0.3),ylab="",xlab="position (kbp)",ylim=c(0,0.15),cex.lab=1.5) #0.2. #"Pi (per bp)"
	mtext(side=2,line=2.5,text=expression(pi[bp]),cex=1.5)
	num.bins<-100
	bins<-rep(1:num.bins,each=length(my.region)/num.bins)
	bin.means<-tapply(my.region,bins,mean,na.rm=TRUE)
	
	bins.pos<-seq(1,length(my.region),length=num.bins)
	lines(bins.pos,bin.means,col="red",lwd=2)
	
	y.axis<- 5*c(0:5)/100 
	alt.y.axis<- 5*c(0:5)/100 /((1.5e-8)*4)/1e3
	axis(side=4,at=y.axis,label=format(alt.y.axis,dig=2))
	mtext(side=4,line=2,"Coalescent Time, kyrs") # (mu=1e-9)
}

pull.this<- 50e3:(55e3-1) #50e3:(55e3-1)  

layout(t(1:2))
plot_mim("DPRG SLP9G",pull.this)
text(x=3000,y=.14,"Comparing two M. guttatus chrs.",col="red")
plot_mim("CAC9N KootN",pull.this)
text(x=3000,y=.14,"Comparing two M. nasutus chrs.",col="red")

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Mimulus_coalescent_times.pdf")

layout(t(1:2))
start<-min(which(pairwise.focal.mim[["bound"]][,"chr"]=="chr_2"))
pull.this<- start:(start+5e3-1)
plot_mim("CAC6G DPRN",pull.this)
text(x=2500,y=.19,"Comparing M. guttatus (1) to M. nasutus chr.",col="red")

plot_mim("DPRN SLP9G",pull.this)
text(x=2500,y=.19,"Comparing M. guttatus (2) to M. nasutus chr.",col="red")


dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Mimulus_coalescent_times_btwn_species.pdf")
