fitness<-c(0.5*2.5	,	0.8*2.5	,	0.8*1)

fitness/max(fitness)


#selection coeffs
1-fitness/max(fitness)

.375/(.375+.6)


#Numbers from Susan Jonston
#For annual reproductive success, the effect sizes are equivalent to 0.92, 0.89 and 0.56 offspring per year for ++, +P and PP males, respectively.
offspring<-c(0.92, 0.89, 0.56) 

#For annual survival, the effect sizes are equivalent to 0.56, 0.65 and 0.65 chance of surviving to the next year for ++, +P and PP males, respectively.
survival<-c(0.56, 0.65, 0.65)
 layout(t(1:3))
 plot(offspring,axes=FALSE,xlim=c(0.9,3.1),cex=3,pch=19,ylim=c(0.5,1),xlab="",ylab="")
 axis(side=1,at=c(1,2,3),labels=c("Ho+, Ho+","Hop,Ho+","Hop,Hop"),cex.axis=1.4)
 axis(side=2,cex.axis=1.2)
mtext(side=2,"offspring per year",line=3,cex=1.4)
 plot(survival,axes=FALSE,xlim=c(0.9,3.1),cex=3,pch=19,ylim=c(0.5,0.7),xlab="",ylab="")
 axis(side=2,cex.axis=1.2)
 axis(side=1,at=c(1,2,3),labels=c("Ho+, Ho+","Hop,Ho+","Hop,Hop"),cex.axis=1.5)
mtext(side=2,"Probability of surviving to the next year",line=3,cex=1.4)

 plot(survival*offspring,axes=FALSE,xlim=c(0.9,3.1),cex=3,pch=19,xlab="",ylab="",ylim=c(0.3,0.6))
 axis(side=2,,cex.axis=1.2)
 axis(side=1,at=c(1,2,3),labels=c("Ho+, Ho+","Hop,Ho+","Hop,Hop"),cex.axis=1.5,cex.lab=1.5)
mtext(side=2,"Yearly Viability x Fecundity",line=3,cex=1.4)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/Soay_Sheep/Hopping_sheep_all.pdf")
#dev.copy2pdf(file="~/Downloads/Hopping_sheep_all.pdf")

