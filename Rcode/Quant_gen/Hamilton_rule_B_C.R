
B<-1.5
C<-0.5
my.range<-c(-B/2,B/2)
offset<-0

layout((1:2))
par(mar=c(4,1.5,1,1))
#plot(c(0,1),c(-B/2,B/2),type="b",pch=19,xlim=range(0,offset+1),ylim=c(-C,C),axes=FALSE)

plot(c(0,1),c(C/2,-C/2),pch=19,ylim=my.range,axes=FALSE,cex=1.5,xlab="",ylab="",type="b",lwd=2) #xlim=range(0,offset+1)
arrows(x0=1,y0=C/2,y1=-C/2,angle=20,length=0.1,code=3)
text(x=0.92,y=0,"C",cex=2)
axis(side=1,at=0:1,label=0:1)
mtext("Altruistic pheno. of ind. i",side=1,line=2,cex=1.5)
mtext("Fitness of ind. i",side=2,line=0,cex=1.5)
axis(side=2,at=my.range,lab=rep("",2))


plot(offset+c(0,1),c(-B/2,B/2),ylim=my.range,pch=19,axes=FALSE,cex=1.5,xlab="",ylab="",type="b",lwd=2)
arrows(x0=1,y0=-B/2,y1=B/2,angle=20,length=0.1,code=3)
text(x=0.92,y=0,"B",cex=2)
axis(side=1,at=0:1,label=0:1)
mtext("Altruistic pheno. of ind. j",side=1,line=2,cex=1.5)
mtext("Fitness of ind. i",side=2,line=0,cex=1.5)
axis(side=2,at=my.range,lab=rep("",2))

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Response_to_sel/Hamiltons_rule_B_C.pdf")
