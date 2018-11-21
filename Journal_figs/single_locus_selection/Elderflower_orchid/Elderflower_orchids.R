

orchid<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Elderflower_orchid/Elderflower_orchids.csv")
plot(orchid$freq_yellow, orchid$male_success,col="red",ylim=range(orchid[,c("male_success","female_pollination_success")]),cex=1.4,pch=18,type="b",cex.lab=1.4,cex.axis=1.2,xlab="Frequency of Yellow morph",ylab="Relative reproductive success of Yellow",lty=2,lwd=2)
points(orchid$freq_yellow, orchid$female,col="black",cex=1.4,pch=19,type="b",lwd=2)
legend(x="topright",legend=c("Female (% pollinia deposited)", "Male (% pollinia removed)"),pch=c(19,18),cex=1.2)
abline(h=1,col="grey",lwd=2)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/single_locus_selection/Elderflower_orchid/Elderflower_orchids_fitness.pdf")