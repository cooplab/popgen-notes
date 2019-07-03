
A Chromosomal Cline in the Grasshopper Podisma pedestris 

grasshopper<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/grasshopper_cline_barton_hewitt/grasshopper_cline_barton_hewitt.csv")

grasshopper$freq[grasshopper$freq<0]<- 0
grasshopper$freq[grasshopper$freq>1]<- 1

cline.widths<-c(580,880,880,810)
names(cline.widths)<-c("A","B","C","D")
my.pch<-c(21,22,24,25)
names(my.pch)<-c("A","B","C","D")

grasshopper$location_unscaled<- grasshopper$location_scaled * cline.widths[grasshopper$transect]


params<-nls(freq ~  1/(1+exp(-2*(location_unscaled-center)/width) ),data=grasshopper,start=list(width=800,center=0))
params<- summary(params)$parameters[,1]
 
 plot(sort(grasshopper$location_unscaled),1/(1+exp(-2*sort(grasshopper$location_unscaled-params["center"])/params["width"]) ),type="l",col="red",lwd=2,ylim=c(0,1),xlab="Distance (m)",ylab="neo-X Frequency",cex.lab=1.4,cex.axis=1.2)
 
points(grasshopper$location_unscaled,grasshopper$freq,cex=1.5,pch=19) # ,pch=my.pch[grasshopper$transect]) #

pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/grasshopper_cline_barton_hewitt/grasshopper_cline_barton_hewitt.pdf")
## bazykin1969hypothetical