
 library(png)
img<-readPNG("~/Dropbox/Courses/Popgen_teaching_Notes/illustration_images/Quant_gen/Pyrenestes_seedcracker/Pyrenestes_seedcracker_beaks.png"). #https://www.flickr.com/photos/internetarchivebookimages/20416920856/in/photolist-otagrj-xVVxst-wRSxF1-x7b4fQ-x9uxzB/
layout(t(1:2))
par(mar=c(4,4.1,1,1))
plot.new()
plot.window(0:1, 0:1)

usr<-par("usr")  
rasterImage(img, usr[1], usr[3], usr[2], usr[4])


black_bellied<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Smith_black_bellied_seed_cracker/Smith_black_bellied.csv")


black_bellied$count_r<-round(black_bellied$count)
black_bellied$survival_count_r<-round(black_bellied$survival_count)
 #black_bellied$survival_count_r[(nrow(black_bellied)-2):nrow(black_bellied)]<-1 #think last 3 entries are survivors, but hard to work out from graph
# black_bellied$survival_count_r[nrow(black_bellied)-1]<-0

par(mar=c(3,3,1,3))

#a<-barplot(height=black_bellied$count_r,offset=6.25,width=0.25)
#barplot(height=black_bellied$survival_count_r,offset=6.25,width=0.25,add=TRUE,col="black")
a<-barplot(height=black_bellied$count_r,offset=0,width=0.25,space=0,xlim=c(0,4.5),xlab="",ylab="")
barplot(height=black_bellied$survival_count_r,offset=0,width=0.25,space=0,add=TRUE,col="black",xlim=c(0,4.5))
mtext(side=1,"Lower mandible length (mm)",cex=1.4,line=2)
mtext(side=2,"Count",cex=1.4,line=2)
axis(at=a[(1:9)*2,1],side=1,lab=format(black_bellied$lower_mandible_length[(1:9)*2],dig=2))


beak.length<-rep(black_bellied$lower_mandible_length,times=black_bellied$count_r)

survival<-apply(black_bellied,1,function(pheno.bin){
	c(rep(1,pheno.bin["survival_count_r"]),rep(0,pheno.bin["count_r"]-pheno.bin["survival_count_r"]))
	})

survival.data<-data.frame(beak.length=beak.length,survival=unlist(survival))

#my.model<-glm(survival~beak.length + I(beak.length^2), family = binomial, data=survival.data)
#range.beak<-seq(6,12,length=100)
#predicted.survival<-predict(my.model,list(beak.length = range.beak),type = "response")
#par(new=TRUE)
#plot(range.beak,predicted.survival,type="l",axes=FALSE)
#axis(4)


binned.data<-data.frame(prob.survival=black_bellied$survival_count_r/black_bellied$count_r,beak.length=black_bellied$lower_mandible_length)

my.model.bin<-glm(prob.survival~beak.length+I(beak.length^2),data=binned.data,family = binomial,weight=black_bellied$count_r)

range.beak<-seq(6,12,length=100)
#plot(range.beak,predict(my.model.bin,list(beak.length = range.beak),type = "response"),type="l",ylim=c(0,1),col="red",lwd=2)

par(new=TRUE)
plot(a[,1],predict(my.model.bin,type = "response"),type="l",ylim=c(0,1),col="red",lwd=2,xlim=c(0,4.5),axes=FALSE)
 axis(4)

mtext(side=4,"Prob. Survival",cex=1.4,line=2)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Smith_black_bellied_seed_cracker/Smith_black_bellied.pdf")

# my.loess<-loess(survival~beak.length,survival.data, span=0.5,control=loess.control(surface="direct"))
# lines(survival.data$beak.length,predict(my.loess),col="red") #,data.frame(beak.length = range.beak)
# axis(4)

a<-sum(black_bellied$count_r*black_bellied$lower_mandible_length^2)/sum(black_bellied$count_r)
b<-(sum(black_bellied$count_r*black_bellied$lower_mandible_length)/sum(black_bellied$count_r))^2

c<-  sum(black_bellied$survival_count*black_bellied$lower_mandible_length^2)/sum(black_bellied$survival_count)
d<-(sum(black_bellied$survival_count*black_bellied$lower_mandible_length)/sum(black_bellied$survival_count))^2
