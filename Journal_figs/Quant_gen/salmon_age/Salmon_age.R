

##data from https://www.biorxiv.org/content/biorxiv/early/2015/08/17/024695.full.pdf

 salmon_age<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/salmon_age/Salmon_age.csv")
 
 salmon_age_mean<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/salmon_age/Salmon_age_means.csv")
 
 salmon_age_fem<-salmon_age[salmon_age$sex=="Female",]
 
rad.cols<- grep("rad",colnames(salmon_age))
 
 radius_fem<-matrix(nrow=3,ncol=3)
 radius_fem[1,]<- as.numeric(salmon_age_fem[2,rad.cols]-salmon_age_fem[1,rad.cols])
  radius_fem[2,]<-as.numeric(salmon_age_fem[4,rad.cols]-salmon_age_fem[3,rad.cols])
  radius_fem[3,]<-as.numeric(salmon_age_fem[6,rad.cols]-salmon_age_fem[5,rad.cols])
 
 area_fem<-radius_fem^2
fem_age_means<-sapply(1:3,function(i){
	sum(area_fem[,i]*c(1:3)/sum( area_fem[,i])) 
})


 salmon_age_mal<-salmon_age[salmon_age$sex=="Male",]
 
rad.cols<- grep("rad",colnames(salmon_age))
 
 radius_mal<-matrix(nrow=3,ncol=3)
 radius_mal[1,]<- as.numeric(salmon_age_mal[2,rad.cols]-salmon_age_mal[1,rad.cols])
  radius_mal[2,]<-as.numeric(salmon_age_mal[4,rad.cols]-salmon_age_mal[3,rad.cols])
  radius_mal[3,]<-as.numeric(salmon_age_mal[6,rad.cols]-salmon_age_mal[5,rad.cols])
 
 area_mal<-radius_mal^2
mal_age_means<-sapply(1:3,function(i){
	sum(area_mal[,i]*c(1:3)/sum( area_mal[,i])) 
})
layout(t(1:2))
par(mar=c(4,4,2,1))
plot(0:2,salmon_age_mean$geno_mean[salmon_age_mean$sex=="Male"],cex=1,ylab="Age at maturity",xlab="Genotype",axes=FALSE,main="Males",cex.lab=1.4,pch=19,xlim=c(-0.5,2.5),  ylim=range(salmon_age_mean$geno_mean[salmon_age_mean$sex=="Male"])*c(0.9,1.1),cex.main=1.4) #6*sqrt(colSums(area_mal)/sum(area_mal))
	symbols(x=0:2,y=salmon_age_mean$geno_mean[salmon_age_mean$sex=="Male"],circles=0.7*sqrt(colSums(area_mal)/sum(area_mal)),bg=adjustcolor("blue",0.2),add=TRUE,inches=FALSE) 

axis(2)
axis(side=1,at=0:2,lab=c("EE","LE","LL"),cex=1.4)

calc_mal_age_means<-salmon_age_mean$geno_mean[salmon_age_mean$sex=="Male"]
names(calc_mal_age_means)<-c(0,1,2)
genos<-rbinom(10000,0:2,p=colSums(area_mal)/sum(area_mal))
phenos<-  calc_mal_age_means[as.character(genos)]
abline(lm(phenos~genos),col="red",lwd=2)

plot(0:2,salmon_age_mean$geno_mean[salmon_age_mean$sex=="Female"],axes=FALSE,ylab="Age at maturity",xlab="Genotype",main="Females",cex.lab=1.4,pch=19,xlim=c(-0.5,2.5), ylim=range(salmon_age_mean$geno_mean[salmon_age_mean$sex=="Female"])*c(0.9,1.1),cex.main=1.4) #cex=6*sqrt(colSums(area_fem)/sum(area_fem))
	symbols(x=0:2,y=salmon_age_mean$geno_mean[salmon_age_mean$sex=="Female"],circles=0.7*sqrt(colSums(area_fem)/sum(area_fem)),bg=adjustcolor("blue",0.2),add=TRUE,inches=FALSE) 

axis(2)
axis(side=1,at=0:2,lab=c("EE","LE","LL"),cex=2)

calc_fem_age_means<-salmon_age_mean$geno_mean[salmon_age_mean$sex=="Female"]
names(calc_fem_age_means)<-c(0,1,2)
genos<-rbinom(10000,0:2,p=colSums(area_fem)/sum(area_fem))
phenos<-  calc_fem_age_means[as.character(genos)]
abline(lm(phenos~genos),col="red",lwd=2)

dev.copy2pdf(file="Journal_figs/Quant_gen/salmon_age/Salmon_age_dom.pdf")