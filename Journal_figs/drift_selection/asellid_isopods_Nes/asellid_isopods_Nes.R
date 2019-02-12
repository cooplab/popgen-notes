isopods_Nes<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/asellid_isopods_Nes/asellid_isopods_Nes.csv")

plot(x=1:2,y=range(isopods_Nes$dNdS),type="n",axes=FALSE,ylab="dN/dS",xlab="",cex.lab=1.4)
axis(2,cex.axis=1.2,at=seq(0.2,0.6,length=5))
sapply(1:11,function(mypair){
	lines(isopods_Nes$dNdS[mypair*2-1:0],type="b",pch=19)
	})
	mtext(1,at=c(1.05,1.95),text=c("Subterranean","Surface"),cex=1.2)
	
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/asellid_isopods_Nes/asellid_isopods_Nes.pdf")	