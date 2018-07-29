
#https://academic.oup.com/jmammal/article/92/4/751/887640
#Genetic Diversity and Fitness in Black-Footed Ferrets Before and During a Bottleneck 
#S. M. Wisely S. W. Buskirk M. A. Fleming D. B. McDonald E. A. Ostrander 

#another set of numbers https://academic.oup.com/jmammal/article/92/4/751/887640
#In 1985 the last wild population (N = 40 adults) experienced simultaneous epizootics of canine distemper and sylvatic plague (Yersinia pestis). 
#. Eighteen individuals were captured for breeding
#
black_footed<-read.csv("Journal_figs/genetic_drift/black_footed_ferrets/black-footed-ferrets_He.csv")

black_footed[,1]<-c(1891,1972,1985,1986) ##displace postbottleneck pop 1 year
#black_footed<-rbind(black_footed,cbind(c(0.067,0.067),c(1999,2004))

plot(black_footed,type="b",xlab= "Year", ylab="Heterozygosity (HE)",ylim=c(0,.3),cex.lab=1.4,cex=1.5,pch=19,range(black_footed$date)+c(-12,5),axes=FALSE)
N<-c("N>10k","N=62","N=40","N=7")
axis(1)
axis(2)

text(black_footed$date-6,black_footed$He-0.008,paste(" (",N,")",sep="")) #black_footed$date)

dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/genetic_drift/black_footed_ferrets/black_footed_ferrets_He.pdf")