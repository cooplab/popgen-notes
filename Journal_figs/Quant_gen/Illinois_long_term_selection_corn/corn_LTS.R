

#data downloaded from
#https://www.ideals.illinois.edu/handle/2142/3525

corn_LTS<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/timmed_corn_LTS_table.csv",head=TRUE,as.is=TRUE,na.strings=".")

##Oil means
plot(corn_LTS[,"YR"],corn_LTS[,"IHO.1"],type="l",ylim=c(0,25),xlab="Year",ylab="% Oil content",cex.axis=1.2,cex.lab=1.4,lwd=2)
abline(lm(corn_LTS[,"IHO.1"]~corn_LTS[,"YR"]),col="red",lwd=1.5)
lines(corn_LTS[,"YR"],corn_LTS[,"ILO.1"],type="l",lty=2,lwd=2)

lines(corn_LTS[,"YR"],corn_LTS[,"IRHO.1"],type="l",lty=3,lwd=2)
lines(corn_LTS[,"YR"],corn_LTS[,"IRLO.1"],type="l",lty=4,lwd=2)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/Illinois_LTS_means.pdf")

##Protein means

plot(corn_LTS[,"YR"],corn_LTS[,"IHP.1"],type="l",ylim=c(0,25),xlab="Year",ylab="% Oil content",cex.axis=1.2,cex.lab=1.4,lwd=2)
abline(lm(corn_LTS[,"IHP.1"]~corn_LTS[,"YR"]),col="red",lwd=1.5)
lines(corn_LTS[,"YR"],corn_LTS[,"ILP.1"],type="l",lty=2,lwd=2)

lines(corn_LTS[,"YR"],corn_LTS[,"IRHP.1"],type="l",lty=3,lwd=2)
lines(corn_LTS[,"YR"],corn_LTS[,"IRLP.1"],type="l",lty=4,lwd=2)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/Illinois_LTS_means.pdf")

####Plots for distribution of individuals

corn.ids<-read.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/LTINDIVIDUAL_EAR_DATA1896_2004A.csv",head=TRUE,as.is=TRUE,na.strings=".",comment.char = "#",skip=15)


###ridge plot of distributions over time
library("ggplot2")
library("ggridges")

 tmp1<-cbind(corn.ids[!is.na(corn.ids$IHO),c("Year","IHO")],"IHO"); colnames(tmp1)<-c("Year","Oil","Exp")
 tmp2<-cbind(corn.ids[!is.na(corn.ids$ILO),c("Year","ILO")],"ILO"); colnames(tmp2)<-c("Year","Oil","Exp")
tmp3<-cbind(corn.ids[!is.na(corn.ids$IRHO),c("Year","IRHO")],"IRHO"); colnames(tmp3)<-c("Year","Oil","Exp")
tmp4<-cbind(corn.ids[!is.na(corn.ids$IRHO),c("Year","IRLO")],"IRLO"); colnames(tmp4)<-c("Year","Oil","Exp")

pheno.sel<-rbind(tmp1,tmp2) #,tmp3,tmp4)
pheno.sel$Exp<-as.factor(pheno.sel$Exp)


ggplot(pheno.sel, aes(x = Oil, y = Year,group=paste(Year,Exp),  fill=Exp)) + 
 #geom_point()
geom_density_ridges(alpha = .5,scale = 10, size = 0.25, rel_min_height = 0.03)+
  theme_ridges(grid = FALSE) +
  # scale_y_reverse(breaks=c(2000, 1980, 1960, 1940, 1920, 1900), expand = c(0.01, 0))+
  scale_x_continuous(limits=c(0, 25), expand = c(0.01, 0)) +
 scale_fill_manual(values =c("#ff8080", "#8080ff"),labels = c("IHO","ILO")) +   #c("red","blue","purple"), labels = c("IHO", "ILO","IRHO"))+
  labs(
  x="Oil content (%)",
  y="Year",
  title="Illinois long term selection experiment"
  )
dev.copy2pdf("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/Illinois_LTS_ggridges_distribution.pdf")


##Protein levels
 tmp1<-cbind(corn.ids[!is.na(corn.ids$IHO),c("Year","IHP")],"IHP"); colnames(tmp1)<-c("Year","Prot","Exp")
 tmp2<-cbind(corn.ids[!is.na(corn.ids$ILO),c("Year","ILP")],"ILP"); colnames(tmp2)<-c("Year","Prot","Exp")

pheno.sel<-rbind(tmp1,tmp2) #,tmp3,tmp4)
pheno.sel$Exp<-as.factor(pheno.sel$Exp)


ggplot(pheno.sel, aes(x = Prot, y = Year,group=paste(Year,Exp),  fill=Exp)) + 
 #geom_point()
geom_density_ridges(alpha = .5,scale = 10, size = 0.25, rel_min_height = 0.03)+
  theme_ridges(grid = FALSE) +
  # scale_y_reverse(breaks=c(2000, 1980, 1960, 1940, 1920, 1900), expand = c(0.01, 0))+
  scale_x_continuous(limits=c(0, 40), expand = c(0.01, 0)) +
 scale_fill_manual(values =c("#ff8080", "#8080ff"),labels = c("IHP","ILP")) +   #c("red","blue","purple"), labels = c("IHO", "ILO","IRHO"))+
  labs(
  x="Protein",
  y="Year",
  title="Illinois long term selection experiment"
  )

###Breeder's equation plot

layout(1:2); 
par(mar=c(3,3,1,1))
year=1897
all.inds<-corn.ids[corn.ids$Year==year & !is.na(corn.ids$IHO),"IHO"]
sel.inds<-corn.ids[corn.ids$Year==year & corn.ids$INIHO==1 & !is.na(corn.ids$INIHO),"IHO"]
kids<-corn.ids[corn.ids$Year==year+1 & !is.na(corn.ids$IHO),"IHO"]

my.range<-range(c(all.inds,sel.inds,kids),na.rm=TRUE)

a<-hist(all.inds,main=year,breaks=seq(my.range[1],my.range[2],length=20)) 
 abline(v=mean(all.inds),col="black",lwd=2)
 abline(v=mean(sel.inds),col="blue",lwd=2)
b<-hist(sel.inds,breaks=a$breaks,add=TRUE,col="blue")

	arrows(x0=mean(all.inds),x1=mean(sel.inds),y0=max(a$counts)*0.8,y1=max(a$counts)*0.8,col="blue",lwd=2,length=0.1,code=3)
	text(x=(mean(all.inds)+mean(sel.inds))/2, y=max(a$counts)*0.9,"S",col="blue",cex=1.5)
legend(x="topleft","Parental gen.",cex=1.4)

c<-hist(kids,main="",breaks=a$breaks)
 abline(v=mean(all.inds),col="black",lwd=2)
 abline(v=mean(kids),col="red",lwd=2)
 abline(v=mean(sel.inds),col="blue",lwd=2)

	arrows(x0=mean(all.inds),x1=mean(kids),y0=max(c$counts)*0.8,y1=max(c$counts)*0.8,col="red",lwd=2,length=0.1,code=3)
	text(x=(mean(all.inds)+mean(kids))/2, y=max(c$counts)*0.9,"R",col="red",cex=1.5)
legend(x="topleft","Offspring gen.",cex=1.4)
mtext(side=1,"Oil content (%)",line=2,cex=1.4)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/Illinois_LTS_breeders_eq.pdf")

###animation 
my.breaks=seq(0,max(corn.ids[!is.na(corn.ids$IHO),"IHO"]),length=35)

system("rm ~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/gif_pngs/*")
gif.dir<-"~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/gif_pngs/"
gen<-1
file.prefix<-"Illinois_up_oil_"
for(year in seq(1896,2000,by=1)){
if(sum(corn.ids$Year==year & !is.na(corn.ids$IHO))!=0){
png(file=paste(gif.dir,file.prefix,year,".png",sep=""))	
gen<-1+gen
a<-hist(corn.ids[corn.ids$Year==year & !is.na(corn.ids$IHO),"IHO"],breaks=my.breaks,freq=FALSE,ylim=c(0,1),title=year,plot=FALSE) #ylim=c(0,30),
b<-hist(corn.ids[corn.ids$Year==year & corn.ids$INIHO==1 & !is.na(corn.ids$INIHO),"IHO"],breaks=my.breaks,add=TRUE,col="blue",freq=FALSE,plot=FALSE)
a$counts<-  a$counts*60/sum(a$counts)
b$counts<-  b$counts*60/sum(a$counts)
plot(a,ylim=c(0,40),main=year,,xlab="Oil content (%)",ylab="Num. inds",cex.axis=1.2,cex.lab=1.4,cex.main=1.4)
legend(x="topright",fill=c("white","blue"),legend=c("All","Selected"),cex=1.2)
plot(b,add=TRUE,ylim=c(0,40),col="blue")
dev.off()

}
}
system(paste("convert -delay 30 $(ls -v ", gif.dir, file.prefix,"*.png) ", gif.dir,file.prefix, "output.gif",sep=""))


##Variance 
 plot(unique(corn.ids$Year), tapply(corn.ids$IHO,corn.ids$Year,var,na.rm=TRUE),ylim=c(0,4),col="red",pch=19,type="b",xlab="Year",ylab="Variance",cex.axis=1.2,cex.lab=1.4)
 points(unique(corn.ids$Year), tapply(corn.ids$ILO,corn.ids$Year,var,na.rm=TRUE),ylim=c(0,4),col="blue",pch=19,type="b") 
 points(unique(corn.ids$Year), tapply(corn.ids$IRHO,corn.ids$Year,var,na.rm=TRUE),ylim=c(0,4),col="purple",pch=19,type="b")
 points(unique(corn.ids$Year), tapply(corn.ids$IRLO,corn.ids$Year,var,na.rm=TRUE),ylim=c(0,4),col="light blue",pch=19,type="b")
  #c("#D55E0050","#ff0000", "#0072B250")
  #scale_color_manual(values = c("#ff0000","#D55E00", "#0072B2"), guide = "none")


