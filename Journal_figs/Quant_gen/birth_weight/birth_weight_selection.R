

birth_weight<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/birth_weight/Karn_Penrose_birth_weight.csv")
birth_weight$death<-birth_weight$deaths_male + birth_weight$deaths_female; 

 plot.fitness.landscape(d=d,fitness.ind.surf=fitness.ind.surf,my.mean=10,sd=2,xrange=c(4,40),wbar=NULL,add.legend=TRUE)
birth_weight$total<-birth_weight$total_female + birth_weight$total_male;


par(mar=c(4,4,1,4))
barplot(height=birth_weight$total,offset=1,width=0.5,xlab="",ylab="",cex.lab=1.4,cex.axis=1.2)
axis(1);box()
mtext("Number of births",side=2,line=2.5,cex=2)
mtext("Birth Weight (lb)",side=1,line=2.5,cex=2)

abline(v=sum((birth_weight$birth_weight+.5)*birth_weight$total)/sum(birth_weight$total),lwd=2)

par(new=TRUE)
plot(birth_weight$birth_weight,birth_weight$death/birth_weight$total,log="y",axes=FALSE,pch=19,xlab="",ylab="",cex=1.5)
axis(4,cex.axis=1.2)
mtext("Mortality",side=4,line=2.5,cex=2)
birth_weight$death/birth_weight$total
these<-(birth_weight$total>0 & birth_weight$death>0) & birth_weight$birth_weight>4

my_birth_weight<-birth_weight$birth_weight[these];
log.fitness<-log(birth_weight$death/birth_weight$total)[these]

my.fit<-nls(log.fitness~B*(my_birth_weight-A)^2 + C, start=list(A=10,B=0.1,C=-4),weights=birth_weight$total[these])
param.fit<-my.fit$m$getPars()
lines(birth_weight$birth_weight,exp(param.fit["B"] * (birth_weight$birth_weight - param.fit["A"])^2 +param.fit["C"]) ,col="red",lwd=2)
#abline(v=param.fit["A"],lwd=2,lty=2)
dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/birth_weight/Karn_Penrose_birth_weight.pdf")

###numbers for reduction in variance

##variance in live births
a<-sum((birth_weight$birth_weight+.5)^2*(birth_weight$total-birth_weight$death))/sum(birth_weight$total-birth_weight$death) ##E(X^2)
b<-(sum((birth_weight$birth_weight+.5)*(birth_weight$total-birth_weight$death))/sum(birth_weight$total-birth_weight$death))^2 ## E(X)^2
print(a-b)

#variance in all births
c<-sum((birth_weight$birth_weight+.5)^2*birth_weight$total)/sum(birth_weight$total)
d<-(sum((birth_weight$birth_weight+.5)*birth_weight$total)/sum(birth_weight$total))^2
print(c-d)

