
primrose<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/evening_primrose/evening_primrose_omega.csv",as.is=TRUE,head=FALSE)
 my.cols<-c("blue","red");names(my.cols)<-c("sex","asex");

 plot(primrose$V4,pch=19,cex=2,col=my.cols[primrose$V2],cex.lab=1.4,cex.axis=1.2,xlab="",ylab="dN/dS",axes=FALSE)
 axis(side=2,cex.axis=1.2)
 for(i in 1:7){ 
 	abline(v=2*i+0.5,col="grey")
 	mtext(side=1,text=paste("C",i,sep=""),at=2*i-0.5,cex=1.4,line=1)
 	}
 legend(x="topright",legend=names(my.cols),pch=19,col=my.cols,cex=1.4,bg="white")
 	
 dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/evening_primrose/evening_primrose_omega.pdf")