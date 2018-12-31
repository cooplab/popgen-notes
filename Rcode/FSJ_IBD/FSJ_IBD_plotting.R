

# IBS$pair<-paste(IBS$IID1,IBS$IID2)
#map<-match(IKD$pair1,IBS$pair)
#IKD$Z1<-IBS$Z1[map]
#IKD$Z2<-IBS$Z2[map]
# IBD<-IKD[!is.na(IKD$Z1),c("Z1","Z2","type_final")]
# write.table(file="~/Dropbox/Courses/PBGG_Core/Popgen_teaching_Notes/Rcode/FSJ_IBD/FSJ_IBD.txt",IBD,quote=FALSE,row.names=FALSE)

IBD<-read.table(file="~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/FSJ_IBD/FSJ_IBD.txt",head=TRUE,as.is=TRUE)
col_pal <- c("#e31a1c",  "#fb99b1",	"#33a02c", "#164413",	"#b2df8a"	,"#680bcc",	"#6a3d9a",	"#cab2d6",	"#fdbf6f",	 "#ff7f00", "#ffe900","#fffac4",	"#a6cee3",	"#1c455a","#ff4f4f")

 names(col_pal)<-c("PO","FS","G1","HS","AV","DC1","G2","GAV","C1","AV2","C1r1","C2","C2r1","C3","C4")
 include.these<-c("PO","FS","G1","HS","AV","C1","AV2")
 
#col_pal["PO"]<-adjustcolor("#e31a1c",0.1)
 col_pal[!(names(col_pal) %in% include.these)]<-NA
 plot(IBD$Z1[is.na(IBD$type_final)],IBD$Z2[is.na(IBD$type_final)],
 xlab="Estimated IBD r1",ylab="Estimated IBD r2",xlim=c(0,1),ylim=c(0,1),col=adjustcolor("black",0.2),pch=19)
 points(IBD$Z1,IBD$Z2,col=adjustcolor(col_pal[IBD$type_final],0.6),pch=19)
 legend(x="topright",legend=c("Parent-Offspring","Full Sib", "Grandparent","1/2 siblings","Aunt/Uncle","GreatAunt/Uncle"),col=col_pal[include.these],pch=19)
 
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/FSJ_IBD.pdf") 
 
 