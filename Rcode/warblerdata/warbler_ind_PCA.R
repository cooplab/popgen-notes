
library("maps")

show(load("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Rcode/warblerdata/warbler_ind_data.Robj"))
mc_warbler<-apply(warbler.ind.allele.counts,2,function(SNP){SNP - mean(SNP)})
 warbler_cov<-cov(t(mc_warbler))
 warbler_eig<-eigen(warbler_cov)


show(load("~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/warblerdata/warb.cols.Robj"))

warbler.ind.coords<-as.data.frame(warbler.ind.coords)

rownames(warbler.ind.coords)<-gsub(" ", "", rownames(warbler.ind.coords), fixed = TRUE)
warbler.ind.coords$pop<-sapply(rownames(warbler.ind.coords),function(my.sample){strsplit(my.sample,"-")[[1]][1]})


warb.sbspp.cols<-adjustcolor(warb.sbspp.cols,0.8)
names(warb.sbspp.cols)<-c("Ni","Vir","Lud","Tro","Obs","Plu")


map(xlim=range(warbler.ind.coords$V1)*c(0.9,1.2),ylim=range(warbler.ind.coords$V2)*c(0.75,1.05))
points(jitter(warbler.ind.coords$V1),jitter(warbler.ind.coords$V2),col=warb.sbspp.cols[warbler.ind.coords$pop],pch=19)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/warbler_PCA_figs/warbler_geo_map.pdf")

my.heatmap<-heatmap(warbler_cov,ColSideColors=warb.sbspp.cols[warbler.ind.coords$pop],RowSideColors=warb.sbspp.cols[warbler.ind.coords$pop],keep.dendro=FALSE)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/warbler_PCA_figs/warbler_heatmap.pdf")



 image(x=1:95,y=1:95,z=warbler_cov[my.heatmap$rowInd,my.heatmap$rowInd],axes=FALSE)
 axis(1,at=1:95, label=warbler.ind.coords$pop[my.heatmap$rowInd],las=2,tick=FALSE,cex=0.5,col.axis=warb.sbspp.cols[warbler.ind.coords$pop][my.heatmap$rowInd])
 
layout(t(1:2))

plot(warbler_eig$vectors[,1],warbler_eig$vectors[,2],col=warb.sbspp.cols[warbler.ind.coords$pop],pch=19,xlab="Principal comp. 1",ylab="Principal comp. 2",cex.lab=1.4,cex=1.1)

#dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/warbler_PCA_figs/warbler_PCAmap.pdf")