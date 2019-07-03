
# http://seger.biology.utah.edu/Seger_Brockmann_87.pdf

PQ <- c(0.1, 0.9)
w<-matrix(NA,nrow=2,ncol=2)

probs<-c(1/2,1/2)

 w[1,] <- c(1,0.785);w[2,] <- c(0.58,0.785);  #Seger setup
#w[1,] <- c(1.2,1);w[2,] <- c(0.85,1);

cat(w[1,1]^(probs[1])*w[2,1]^(probs[2]),(w[1,1]*probs[1])+(w[2,1]*probs[2]))
cat(w[1,2]^(probs[1])*w[2,2]^(probs[2]),(w[1,2]*probs[1])+(w[2,2]*probs[2]))

plot(x=c(1,100),y=c(0,0.25),type="n",xlab="Generations",ylab="Frequency",cex.axis=1.2,cex.lab=1.4)
PQ <- c(0.1, 0.9);p<-numeric()
for(gen in 1:200){
my.environ<-sample(1:2,1,prob=probs) #(gen %% 2)+1
p<-c(p,PQ[1]/sum(PQ))
PQ<-w[my.environ,]*PQ
if(my.environ==2) polygon(c(gen,gen+1,gen+1,gen),c(0,0,1,1),col=adjustcolor("red",0.2),border=FALSE)
}

lines(p,lwd=2)

#pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/figures/Haploid_geom_traj.pdf")