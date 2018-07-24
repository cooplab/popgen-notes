stalk_eyed.1<-read.csv("Journal_figs/Quant_gen/stalk_eyed_flies/Selection_allometry_cleaned.csv")


layout(t(1:2))
plot(c(0,10),rowMeans(stalk_eyed.1[stalk_eyed.1$sex=="male",-1])[1]+c(-0.1,0.1),type="n",xlab="generations",ylab="Eye-span/body length",cex.lab=1.5)

down.males<-rowMeans(stalk_eyed.1[stalk_eyed.1$sex=="male",grep("down",colnames(stalk_eyed.1))])
up.males<-rowMeans(stalk_eyed.1[stalk_eyed.1$sex=="male",grep("up",colnames(stalk_eyed.1))])
control.males<-rowMeans(stalk_eyed.1[stalk_eyed.1$sex=="male",grep("control",colnames(stalk_eyed.1))])
points(0:10,down.males,type="b",pch=25)
points(0:10,up.males,type="b",pch=24)
points(0:10,control.males,type="b",pch=21)
legend(x="topleft","Males",cex=1.5)

plot(c(0,10),rowMeans(stalk_eyed.1[stalk_eyed.1$sex=="female",-1])[1]+c(-0.1,0.1),type="n",xlab="generations",ylab="Eye-span/body length",cex.lab=1.5)

down.females<-rowMeans(stalk_eyed.1[stalk_eyed.1$sex=="female",grep("down",colnames(stalk_eyed.1))])
up.females<-rowMeans(stalk_eyed.1[stalk_eyed.1$sex=="female",grep("up",colnames(stalk_eyed.1))])
control.females<-rowMeans(stalk_eyed.1[stalk_eyed.1$sex=="female",grep("control",colnames(stalk_eyed.1))])
points(0:10,down.females,type="b",bg="black",pch=25)
points(0:10,up.females,type="b",bg="black",pch=24)
points(0:10,control.females,type="b",bg="black",pch=21)
legend(x="topleft","Females",cex=1.5)

dev.copy2pdf(file="Journal_figs/Quant_gen/stalk_eyed_flies/stalk_eyed_flies_response.pdf")