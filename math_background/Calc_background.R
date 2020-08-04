library("latex2exp")
library("RColorBrewer")

  fx<-function(x){(-x^4/3 -(5/6)*x^3 +x)}
 dy.dx<-function(x){ -4*x^3/3- (5/6)*3*x^2+1}
 d2y.dx2<-function(x){ -3*4*x^2/3- 2*(5/6)*3*x}

 x<-seq(-2,1,length=1000)
peaks<-x[which(abs(dy.dx(x))<0.003)]
btwn.peaks<-c(min(x),peaks)+ diff(c(min(x),peaks,max(x)))/2

###PLOT FUNCTION AND DERIVATIVE
layout(1:3)
par(mar=c(2,4,1,1))

plot(x,fx(x),type="l",lwd=2,axes=FALSE,xlab="",ylab="")
mtext(at=btwn.peaks, side=1,c("A","B","C","D"),cex=1.3,line=1.5)
abline(h=0)
abline(v=peaks,lty=2,lwd=2)
axis(2,cex=1.2)
#mtext("x",side=1,line=2.4,cex=1.4)
mtext(TeX("$f(x)$"),side=2.4,line=2,cex=1.4)

par(mar=c(4,4,1,1))

plot(x, dy.dx(x), lwd=2,type="l",axes=FALSE,xlab="",ylab="")
abline(h=0)
abline(v=peaks,lty=2,lwd=2)
axis(2,cex=1.2)
#mtext("x",side=1,line=2,cex=1.4)
mtext(TeX("$f'(x)$"),side=2.4,line=2,cex=1.4)
plot(x, d2y.dx2(x), lwd=2,type="l",axes=FALSE,xlab="",ylab="")
abline(h=0)
# abline(v=x[which(abs(dy.dx(x))<0.003)],lty=2,lwd=2)
axis(2,cex=1.2,at=c(-8,-6,-4,-2,0,2))
abline(v=peaks,lty=2,lwd=2)

mtext(TeX("$f''(x)$"),side=2.4,line=2,cex=1.4)
axis(1,cex=1.2)
mtext("x",side=1,line=2.5,cex=1.4)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/calc_pics/Derivat_all_3.pdf")
#dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/calc_pics/Derivat.pdf")

###PLOT SECOND DERIVATIVE AS ITS OWN PANEL, OLD
layout(1)
par(mar=c(4,4,1,1))

 plot(x, d2y.dx2(x), lwd=2,type="l",axes=FALSE,xlab="",ylab="")
 abline(h=0)
# abline(v=x[which(abs(dy.dx(x))<0.003)],lty=2,lwd=2)
   axis(2,cex=1.2)
    abline(v=peaks,lty=2,lwd=2)

mtext(TeX("$f''(x)$"),side=2.4,line=2,cex=1.4)
axis(1,cex=1.2)
mtext("x",side=1,line=2.5,cex=1.4)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/calc_pics/2nd_Derivat.pdf")

plot.taylor.1<-function(a){
	taylor.x<- seq(a-0.25,a+0.25,length=1000)
	taylor.1<-fx(a) + (taylor.x-a) * dy.dx(a)
	lines(taylor.x,taylor.1,col="red",lwd=2)
	points(a,fx(a),pch=19,cex=1.4,col="red")
}

plot.taylor.2<-function(a){
	taylor.x<- seq(a-0.25,a+0.25,length=1000)
	taylor.2<-fx(a) + (taylor.x-a) * dy.dx(a) + (taylor.x-a)^2 * d2y.dx2(a)/2
	lines(taylor.x,taylor.2,col="red",lwd=2)
	points(a,fx(a),pch=19,cex=1.4,col="red")
}

par(mar=c(3.5,3.5,1,1))
layout(1)
 plot(x,fx(x),type="l",lwd=2,axes=FALSE,xlab="",ylab="",ylim=c(-0.7,0.45))
 abline(h=0)
  axis(2,cex=1.2)
mtext("x",side=1,line=2,cex=1.4)
mtext(TeX("$f(x)$"),side=2.4,line=2,cex=1.4)
axis(1,cex=1.2)
plot.taylor.1(-0.25)
plot.taylor.1(-1.6)
plot.taylor.1(0.4)
plot.taylor.1(-1)
plot.taylor.1(0.8)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/calc_pics/Taylor_1.pdf")
par(mar=c(3.5,3.5,1,1))
layout(1)
 plot(x,fx(x),type="l",lwd=2,axes=FALSE,xlab="",ylab="",ylim=c(-0.7,0.45))
 abline(h=0)
  axis(2,cex=1.2)
mtext("x",side=1,line=2,cex=1.4)
mtext(TeX("$f(x)$"),side=2.4,line=2,cex=1.4)
axis(1,cex=1.2)
plot.taylor.2(-0.25)
plot.taylor.2(-1.6)
plot.taylor.2(0.4)
plot.taylor.2(-1)
plot.taylor.2(0.8)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/calc_pics/Taylor_2.pdf")

my.cols<-brewer.pal("Dark2",n=3)
y<-fx(x)
plot(x,y,type="l",lwd=2,axes=FALSE,xlab="",ylab="")
 these<-667:978
 polygon(x=c(x[these],rev(x[these])),y=c(y[these],rep(0,length(these))),col=my.cols[2])
 these<-500:667
 polygon(x=c(x[these],rev(x[these])),y=c(y[these],rep(0,length(these))),col=my.cols[3])

 these<-978:1000
 polygon(x=c(x[these],rev(x[these])),y=c(y[these],rep(0,length(these))),col=my.cols[3])
 abline(h=0)
 

  axis(2,cex=1.2)
 axis(1,cex=1.2)
 mtext("x",side=1,line=2.5,cex=1.4)
 mtext(TeX("$f(x)$"),side=2.4,line=2,cex=1.4)
 
 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/math_background/calc_pics/Integral_eg.pdf")
 
 