

#data downloaded from
#https://www.ideals.illinois.edu/handle/2142/3525

corn_LTS<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/timmed_corn_LTS_table.csv",head=TRUE,as.is=TRUE,na.strings=".")

plot(corn_LTS[,"YR"],corn_LTS[,"IHO.1"],type="l",ylim=c(0,25),lwd=2,xlab="Year",ylab="% Oil content",cex.axis=1.2,cex.lab=1.4)

lines(corn_LTS[,"YR"],corn_LTS[,"ILO.1"],type="l",lty=2,lwd=2)

lines(corn_LTS[,"YR"],corn_LTS[,"IRHO.1"],type="l",lty=3,lwd=3)

corn.ids<-read.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/LTINDIVIDUAL_EAR_DATA1896_2004A.csv",head=TRUE,as.is=TRUE,na.strings=".",comment.char = "#",skip=15)

	my.breaks=seq(0,max(corn.ids[!is.na(corn.ids$IHO),"IHO"]),length=50)
hist(corn.ids[corn.ids$Year=="1896" & corn.ids$INIHO==0 & !is.na(corn.ids$INIHO),"IHO"],breaks=my.breaks)

system("rm ~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/gif_pngs/*")
gif.dir<-"~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Illinois_long_term_selection_corn/gif_pngs/"
gen<-1
file.prefix<-"Illinois_up_oil_"
for(year in seq(1896,2000,by=1)){
png(file=paste(gif.dir,file.prefix,year,".png",sep=""))	
gen<-1+gen
hist(corn.ids[corn.ids$Year==year & !is.na(corn.ids$IHO),"IHO"],breaks=my.breaks,freq=FALSE,ylim=c(0,1)) #ylim=c(0,30),
#hist(corn.ids[corn.ids$Year==year & corn.ids$INIHO==1 & !is.na(corn.ids$INIHO),"IHO"],breaks=my.breaks,add=TRUE,col="blue",freq=FALSE)
dev.off()
}
system(paste("convert -delay 20 $(ls -v ", gif.dir, file.prefix,"*.png) ", gif.dir,file.prefix, "output.gif",sep=""))


 tmp1<-cbind(corn.ids[!is.na(corn.ids$IHO),c("Year","IHO")],"IHO"); colnames(tmp1)<-c("Year","Oil","Exp")
 tmp2<-cbind(corn.ids[!is.na(corn.ids$ILO),c("Year","ILO")],"ILO"); colnames(tmp2)<-c("Year","Oil","Exp")
tmp3<-cbind(corn.ids[!is.na(corn.ids$IRHO),c("Year","IRHO")],"IRHO"); colnames(tmp3)<-c("Year","Oil","Exp")
pheno.sel<-rbind(tmp1,tmp2) #,tmp3)
pheno.sel$Exp<-as.factor(pheno.sel$Exp)



ggplot(pheno.sel, aes(x = Oil, y = Year,group=paste(Year,Exp),  fill=Exp)) + 
 #geom_point()
geom_density_ridges(alpha = .5,scale = 10, size = 0.25, rel_min_height = 0.03)+
  theme_ridges() +
   scale_y_reverse(breaks=c(2000, 1980, 1960, 1940, 1920, 1900), expand = c(0.01, 0))+
  scale_x_continuous(limits=c(0, 25), expand = c(0.01, 0)) +
 scale_fill_manual(values =c("#ff8080", "#8080ff"),labels = c("IHO", "ILO")) +   #c("red","blue","purple"), labels = c("IHO", "ILO","IRHO"))+
  labs(
  x="Oil content (%)",
  y="Year",
  title="Illinois long term selection experiment"
  )+
 theme_ridges(grid = FALSE)

 
  #c("#D55E0050","#ff0000", "#0072B250")
  #scale_color_manual(values = c("#ff0000","#D55E00", "#0072B2"), guide = "none")
