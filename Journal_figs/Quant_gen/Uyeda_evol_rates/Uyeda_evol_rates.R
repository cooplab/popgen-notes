
library("latex2exp")
library("RColorBrewer")

#data from https://datadryad.org/resource/doi:10.5061/dryad.7d580
 Uyeda_data<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Uyeda_evol_rates/Dryad7.csv",as.is=TRUE)
 
 
 
 #Gingerich<-read.csv("~/Downloads/Gingerich2019_Rates_Archive_FossilStudies.csv",as.is=TRUE)
 my.cols<-sample(brewer.pal(3,"Dark2"))
 names(my.cols)<-c(unique(Uyeda_data$Data.type),"blah")
 
plot(Uyeda_data$log10.years,log10(abs(Uyeda_data$Darwins..standardized.by.k)),ylim=c(-5,8),col=adjustcolor( my.cols[Uyeda_data$Data.type],0.2),pch=19, xlab=TeX("$years$"),ylab="",cex.axis=1.2,cex.lab=1.4,axes=FALSE) #\\log_{10}(
mtext(side=2,line=2,cex=1.4,TeX("$Abs. pheno. change, Darwins$"))
axis(side=1,at=-1:7,label=parse(text=paste("10^",-1:7,sep="")))
axis(side=2,at=seq(-4,7,by=2),label=parse(text=paste("10^",seq(-4,7,by=2),sep="")))

Dogwinkles<- Uyeda_data[grep("lapillus",Uyeda_data$Species),]
Dogwinkles<- Dogwinkles[which.max(abs(Dogwinkles$Darwins..standardized.by.k.)),c("log10.years","Darwins..standardized.by.k.")]
 points(Dogwinkles[1,1],log10(abs(Dogwinkles[1,2])),pch=19)


# Uyeda_data[grep("Bisonantiquus",Uyeda_data$Species),]
# (log(36) - log(200))/6000
lister.red.deer<-Uyeda_data[grepl("elaphus",Uyeda_data$Species) & Uyeda_data$log10.gens>3,]
lister.red.deer<-lister.red.deer[which.max(abs(lister.red.deer$Darwins..standardized.by.k.)),c("log10.years","Darwins..standardized.by.k.")]
 points(lister.red.deer[1,1],log10(abs(lister.red.deer[1,2])),pch=19)
)

Triceratops<-Uyeda_data[grepl("Protoceratops.Triceratops",Uyeda_data$Species) ,]
Triceratops<-Triceratops[which.max(abs(Triceratops$Darwins..standardized.by.k.)),c("log10.years","Darwins..standardized.by.k.")]
points(Triceratops[1,1],log10(abs(Triceratops[1,2])),pch=19)

dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Uyeda_evol_rates/Uyeda_evol_rates.pdf")

 https://www.pnas.org/content/108/38/15908
 
 
 https://www.flickr.com/photos/biodivlibrary/7115955399
 
 
 buffalos
 Uyeda_data[grep("antiquus",Uyeda_data$Species) & Uyeda_data$Years>30e3,]
 
 https://www.jstor.org/stable/pdf/2405374.pdf?refreqid=excelsior%3A315534eb3ac470ac9829a0d9c0df147f
 colbert dinosaurs
 https://archive.org/stream/annualreporto19261928fiel/#page/326/mode/1up
 
 https://archive.org/stream/bookruli00colb/#page/82/mode/1up