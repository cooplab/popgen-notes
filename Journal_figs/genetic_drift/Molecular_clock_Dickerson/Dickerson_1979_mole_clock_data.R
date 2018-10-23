require ( RColorBrewer )
D.moleclock<-read.csv(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Molecular_clock_Dickerson/Dickerson_1979_mole_clock_data.csv")
time.lab<-grepl("time",colnames(D.moleclock))
subs.lab<-grepl("sub",colnames(D.moleclock))

plot(x=c(0,max(D.moleclock[,time.lab],na.rm=TRUE)),y=c(0,max(D.moleclock[,subs.lab],na.rm=TRUE)),type="n",xlab="Millions of years since divergence",ylab="Corrected # of amino-acid changes (per 100 residues)",cex.lab=1.4,cex.axis=1.2)

my.lms<-list()

my.cols<-brewer.pal(3,"Dark2")
my.genes<-c("cytoc","hemo","Fib")

names(my.cols)<-my.genes

full.gene<-c("Cytochrome c", "Hemoglobin", "Fibrinopeptides")
names(full.gene)<-my.genes

for(gene in my.genes){

	x<-D.moleclock[,grepl(gene,colnames(D.moleclock)) & time.lab]
	
	y<-D.moleclock[,grepl(gene,colnames(D.moleclock)) & subs.lab]
	my.lms[[gene]]<-lm(rowMeans(y)~x[,1]+0)
	points(x[,1],rowMeans(y),,col=my.cols[gene],pch=19)
	print(my.lms)
	my.text<-paste(full.gene[gene],",\n",format(my.lms[[gene]]$coefficients,dig=2)," subs per Myrs",sep="")
	text(x[sum(!is.na(x[,1]))-1,1],rowMeans(y)[sum(!is.na(y[,1]))-1]-5,my.text,pos=4,col=my.cols[gene])
	 abline(a=0,b=my.lms[[gene]]$coefficients,col=my.cols[gene])
	segments(x0=x[,1],x1=x[,1],y0=y[,1],y1=y[,2],col=my.cols[gene])
}

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/Molecular_clock_Dickerson/Dickerson_1979_mole_clock_fig.pdf")