codon.bias<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/Codon_bias_Drosophila/codon_bias.csv")
#data from  http://www.genetics.org/content/160/2/595
plot(codon.bias,pch=19,cex=1.5,xlab="Expression Level",ylab="Codon Bias",cex.lab=1.4,cex.axis=1.2)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/Codon_bias_Drosophila/Drosophila_codon_bias_expression.pdf")

Leucine<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/Codon_bias_Drosophila/Leucine_codon_bias.csv")
par(mar=c(2,4,1,1))
barplot(height=Leucine[Leucine$organism=="Drosophila","freq"],names.arg=Leucine[Leucine$organism=="Drosophila","codon"],ylab="Frequency",cex.lab=1.4,cex.axis=1.2)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/drift_selection/Codon_bias_Drosophila/Leucine_codon_bias.pdf")
#,lab=barplot(Leucine[Leucine$organism=="Drosophila","codon"]))

