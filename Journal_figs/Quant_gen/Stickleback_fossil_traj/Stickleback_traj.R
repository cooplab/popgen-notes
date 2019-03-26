

stickleback_traj<-read.csv("~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Stickleback_fossil_traj/Touching_pterygiophores.csv")

layout((1:2),height=c(0.8,0.3))
par(mar=c(1,3.1,3.1,0.5))
plot(stickleback_traj$touching, -stickleback_traj$gen,pch=19,axes=FALSE,type="b",xlim=c(0.6,1.42))
axis(2,at=-c(0,2500,5000,7500),lab=c(0,2500,5000,7500)*2)
axis(3)
initial<-1.43
opt<-0.82
step<-8.5e-6
alpha<-1.19e-3
t<-stickleback_traj$generation
expected.traj<-(1-exp(-alpha*t))*opt + exp(-alpha*t)*initial
lines(expected.traj,-t,lty=2,lwd=2)
z<-seq(0.6,1.4,length=100)
	mtext(side=3,"Touching Pterygiophores",cex=1.4,line=2)
	mtext(side=2,"Years",cex=1.4,line=2)
omega<-6.7
fitness.surf<-exp(-(z-opt)^2/2*omega)
fitness.traj<-exp(-(stickleback_traj$touching_pterygiophores-opt)^2/2*omega)

par(mar=c(3.1,3.1,1,0.5))
plot(z,fitness.surf,lwd=2,type="l",axes=FALSE,xlim=c(0.6,1.42)) #type="l",
axis(1)
axis(2,at=c(1,0.5))

my.diff<-diff(fitness.traj) #stickleback_traj$touching)
my.diff<-abs(my.diff)/max(abs(my.diff))
for(i in 1:(nrow(stickleback_traj)-1)){ 
	arrows(x0=stickleback_traj$touching[i],x1=stickleback_traj$touching[i+1],y0=fitness.traj[i],y1=fitness.traj[i+1],length=0.25*my.diff[i],col=adjustcolor("black",my.diff[i]),lwd=2)
	}
	mtext(side=2,"Fitness",cex=1.4,line=2)
	mtext(side=1,"Touching Pterygiophores",cex=1.4,line=2)
	
	dev.copy2pdf(file="~/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/Stickleback_fossil_traj/Stickleback_fossil_traj.pdf")