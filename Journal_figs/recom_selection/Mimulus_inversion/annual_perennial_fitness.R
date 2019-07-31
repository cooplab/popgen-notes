
 library(jpg)
img<-readJPG("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/Mimulus_inversion/annual_perennial.png")
layout(t(1:2))
par(mar=c(4,4.1,1,1))
plot.new()
plot.window(0:1, 0:1)

usr<-par("usr")  
rasterImage(img, usr[1], usr[3], usr[2], usr[4])

par(mar=c(4,4.1,1,1))
plot(c(0.416375356,13.216634),type="b",pch=19,lwd=2,cex=1.5,ylab="Fitness",cex.axis=1.2,cex.lab=1.4,axes=FALSE,xlim=c(0.75,2.25),xlab="")
axis(2,cex.axis=1.2,cex.lab=1.4)
mtext(side=1,at=c(1,2),c("Coastal","Inland"),cex=2,line=1)
lines(c(5.804077674,0.155064762),type="b",pch=19,lwd=2,cex=1.5,lty=2)
legend(x="topleft",c("Inland annual","Coastal perennial"),lty=c(1,2),cex=1.1,lwd=2)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/Mimulus_inversion/annual_perennial_fitness.pdf")

inversion_coords<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/recom_selection/Mimulus_inversion/Mimulus_inversion_locs.csv",as.is=TRUE,head=TRUE,skip=1)
return_coord<-function(my.coord){
	tmp1<-strsplit(my.coord, "[°\\'\"”]") #"58° 00' 26\""
	tmp1<-as.numeric(tmp1[[1]])
	return(tmp1[1]+tmp1[2]/60+tmp1[3]/3600) 
}

inversion_coords$New.Lat<-sapply(inversion_coords$Lat,return_coord)


 map(database="state",ylim=c( 32, 55),xlim=c(-135,-110))
 
top = 49.3457868 # north lat

left = -124.7844079 # west long

right = -66.9513812 # east long

bottom =  24.7433195 # south lat
