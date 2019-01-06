#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3186250/
##Dataset captured from figure
father_son_patch<-read.csv("Journal_figs/Quant_gen/pied_fly_catcher_sex_genetic_corr/Potti_Canal_2011_father_son_patch.csv")
father_daughter_patch<-read.csv("Journal_figs/Quant_gen/pied_fly_catcher_sex_genetic_corr/Potti_Canal_2011_father_daughter_patch.csv")

layout(t(1:2))
plot(father_son_patch,xlab="Size of Father's forehead patch",ylab="Size of son's forehead patch",cex.lab=1.5,pch=19)
abline(lm(father_son_patch$sons_forehead~ father_son_patch$father_forehead),lwd=2)

plot(father_daughter_patch,xlab="Size of Father's forehead patch",ylab="Size of Daughter's forehead patch",cex.lab=1.5,pch=19)
abline(lm(father_daughter_patch$daughters_forehead_patch~ father_daughter_patch$father_forehead),lwd=2)

dev.copy2pdf(file="Journal_figs/Quant_gen/pied_fly_catcher_sex_genetic_corr/FlyCatcher_genetic_corr.pdf")
