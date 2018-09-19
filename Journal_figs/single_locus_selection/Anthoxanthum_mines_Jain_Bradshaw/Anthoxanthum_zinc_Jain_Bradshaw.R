#Jain and Bradshaw

###The scale is off, look like on the pic that they've moved 70 on x axis in a bit.

#. https://www.nature.com/articles/hdy196642
zinc_mine<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Anthoxanthum_mines_Jain_Bradshaw/Anthoxanthum_zinc_Jain_Bradshaw.csv")

plot(zinc_mine[,1:2],xlab="Distance to Mine Boundary (meters)",ylab="Zinc Tolerance",cex.lab=1.4, type="n", axes=FALSE)

text(-10,45,"Mine",cex=1.4)
text(15,45,"Off Mine",cex=1.4)
mtext(side=3,at=zinc_mine$PPM_meters,text=zinc_mine$PPM_Zinc,las=2,cex=1.2)
mtext(side=3,at=-20,text="Zn \n(P.P.M.)",cex=1.2)
axis(1,cex.axis=1.4,at=c(-20,-10,0,10,20,30,zinc_mine[nrow(zinc_mine),1]),lab=c(20,10,0,10,20,30,70))
axis(2,cex.axis=1.4)
abline(v=0,col="grey",lwd=3); 
points(zinc_mine[,1:2],pch=19,cex=2,type="b")

dev.copy2pdf(file="Journal_figs/single_locus_selection/Anthoxanthum_mines_Jain_Bradshaw/Anthoxanthum_zinc.pdf")

