

#microsats<-read.table("~/Downloads/HGDP_microsats_withCODIS_978n.stru",as.is=TRUE,skip=1)
#microsats.loci<-read.table("~/Downloads/HGDP_microsats_withCODIS_978n.stru",as.is=TRUE,nrow=1)

#inds<-microsats[,1:5]
#microsats.alleles<-microsats[,-c(1:5)]

#colnames(microsats.alleles)<-microsats.loci

#write.table(file="~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/CODIS/CODIS.txt", cbind(inds,microsats.alleles[,(ncol(microsats.alleles)-12):ncol(microsats.alleles)]),quote=FALSE,row.names=FALSE,sep="\t")



 CODIS<-read.table(file="~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/CODIS/CODIS.txt",as.is=TRUE,head=TRUE)
 
 
 write.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/CODIS/TH01_counts.csv",table(CODIS[CODIS$V5=="EUROPE","TH01"]))
 write.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/CODIS/D16S539_counts.csv",table(CODIS[CODIS$V5=="EUROPE","D16S539"]))
 
  