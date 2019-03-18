
primrose<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/evening_primrose/evening_primrose_omega.csv",as.is=TRUE,head=FALSE)
my.pch<-c(19,18); names(my.pch)<-c("sex","asex");  #c("blue","red");

 plot(primrose$V4,cex=2,pch=my.pch[primrose$V2],cex.lab=1.4,cex.axis=1.2,xlab="",ylab="dN/dS",axes=FALSE)
 axis(side=2,cex.axis=1.2)
 for(i in 1:7){ 
 	abline(v=2*i+0.5,col="grey")
 	mtext(side=1,text=paste("C",i,sep=""),at=2*i-0.5,cex=1.4,line=1)
 	}
 legend(x="topright",legend=names(my.pch),pch=my.pch,cex=1.4,bg="white")
 	
 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/evening_primrose/evening_primrose_omega.pdf")