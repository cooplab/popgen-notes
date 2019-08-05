

%Northwestern garter snake 
% Eutaenia ordinoides


% https://www.sil.si.edu/DigitalCollections/usexex/navigation/scientificatlases/usexex19_31a.cfm?startrecord=16

# [fitness = 4.2 + (0.008 x reversals) + (0.102 x stripe) + (0.017 x reversals2) - (0.034 x stripe2)- (0.268 x reversals x stripe)]


my.reversals<-seq(-1.4,4.6,length=500)
my.stripe<-seq(-1.4,3.4,length=450)

snake.fitness<-function(reversals,stripe){
 	4.2 + (0.008 * reversals) + (0.102 * stripe) + (0.017 * reversals^2) - (0.034 * stripe^2)- (0.268 * reversals * stripe)
}

fitness.surf<-outer(my.reversals,my.stripe,snake.fitness)
par(mar=c(4,4.1,1,1))
layout(t(1:3))
image(fitness.surf,x=my.reversals,y=my.stripe, cex.axis=1.2,cex.lab=1.4,xlab="Reversals",ylab="Stripes")
contour(fitness.surf,x=my.reversals,y=my.stripe,add=TRUE)

all.snakes<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Garter_snakes_Brodie/All_snakes.csv")
 surviving.snakes<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Garter_snakes_Brodie/surviving_snakes.csv")
nlevels=20
contour(fitness.surf,x=my.reversals,y=my.stripe,cex.axis=1.2,cex.lab=1.4,xlab="Reversals",ylab="Stripes") 
points(all.snakes,pch=19,col=adjustcolor("black",0.5))
contour(fitness.surf,x=my.reversals,y=my.stripe,cex.axis=1.2,cex.lab=1.4,xlab="Reversals",ylab="Stripes")
points( surviving.snakes,pch=19,col="black")

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Garter_snakes_Brodie/Garter_snakes_Brodie.pdf")