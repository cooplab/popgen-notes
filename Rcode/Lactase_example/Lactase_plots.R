

 library('RColorBrewer')
LCT<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/Lactase_example/Figure3_LP_data-2017_NRG.csv",as.is=TRUE)


LCT[,1]<-as.numeric(gsub(",","",LCT[,1]))
LCT[nrow(LCT)+1,]<-NA
LCT$Midpoint.Date[nrow(LCT)]<-0
LCT$DAF[nrow(LCT)]<-0.75
LCT$Region[nrow(LCT)]<-"Central Europe"


my.cols<-brewer.pal(n=8,name="Set3")
names(my.cols)<-unique(LCT$Region)[1:8]

 
plot(-LCT$Midpoint.Date,LCT$DAF,col=my.cols[LCT$Region],pch=19)

plot(-LCT$Midpoint.Date[LCT$Region=="Central Europe"],LCT$DAF[LCT$Region=="Central Europe"],pch=19,xlab="Years in Past",ylab="Frequency",xlim=c(min(-LCT$Midpoint),0),cex=1.4,cex.axis=1.2,cex.lab=1.4)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/Lactase_example/Lactase_freq_time.pdf")

logistic.increase<-function(){
	
	
}

nlm()