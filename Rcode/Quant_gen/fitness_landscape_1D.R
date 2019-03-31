library("latex2exp")

num.points<-10000
layout((1:2),heights=c(0.1,0.9))

x<-seq(1,20,length=num.points); y1<-dnorm(x,14,3.5)*1.8; y2<-dnorm(x,5,2); y3<-dnorm(x,19,sd=1)*0.25; 
my.y<-y1+y2+y3; 

par(mar=c(0,3,0.1,1))
my.palette<-colorRampPalette(c("blue","white","red"))
my.diff<-diff(log(my.y))

# plot(my.diff, (1/my.y[-length(my.y)])*diff(my.y)). ##if you're interested to see that two statements are equal


zrange<-c(-max(abs(my.diff)),max(abs(my.diff)))

image(x=x[-num.points],z=t(rbind(my.diff,my.diff)),zlim=zrange,col=my.palette(240),axes=FALSE,xlim=range(x),xaxs="i")
mtext(side=2,TeX("$\\frac{1}{\\bar{W}} \\times \\frac{\\partial \\bar{W}}{\\partial \\bar{z}}$"),las=2,padj=0.6)

par(mar=c(2,3,0.1,1))
plot(x,my.y,type="l",lwd=2,xlim=range(x),axes=FALSE,ylim=c(0,max(my.y)),xaxs="i")
mtext(expression(bar(W)),side=2,line=1.5,cex=1.4) #TeX("Mean Fitness $\bar{W}$"),side=2)
mtext(expression(bar(z)),side=1,line=1,cex=1.4) #TeX("Mean Fitness $\bar{z}$"),side=1)
axis(side=1,label=FALSE,at=c(1,6,16,21))
axis(side=2,label=FALSE)

for(i in 1:18){
	here<-num.points*(i/19)
    if(my.y[here] < my.y[here+1]) arrows(y0=my.y[here],y1=my.y[here+1],x0=x[here],x1=x[here+1],length=0.15)
    if(my.y[here] > my.y[here+1]) arrows(y0=my.y[here+1],y1=my.y[here],x0=x[here+1],x1=x[here],length=0.15)

 }
 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Response_to_sel/fitness_landscape_1D.pdf")