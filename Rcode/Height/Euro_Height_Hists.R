##Code in part written by Jeremy Berg
direct<-"~/Dropbox/Courses/Popgen_teaching_Notes/Rcode/Height/"

height.freqs <- read.table ( paste(direct,"europe.height.freqs.data",sep="") , h = T )
height.effects <- read.table ( paste(direct,"europe.height.163",sep="") , h = T )

french.freqs <- height.freqs [ height.freqs$CLST == "French" , ]
sardinian.freqs <- height.freqs [ height.freqs$CLST == "Sardinian" , ]

french.freqs <- french.freqs [ order ( french.freqs$SNP) , ] 
sardinian.freqs <- sardinian.freqs [ order ( sardinian.freqs$SNP) , ] 

french.mean.height <- sum ( french.freqs$FRQ * height.effects$EFF )
sardinian.mean.height <- sum ( sardinian.freqs$FRQ * height.effects$EFF )
## flip all to positive
flip.these <- height.effects$EFF < 0

french.freqs [ flip.these , ]$FRQ <- 1 - french.freqs [ flip.these , ]$FRQ
sardinian.freqs [ flip.these , ]$FRQ <- 1 - sardinian.freqs [ flip.these , ]$FRQ
height.effects$EFF <- abs ( height.effects$EFF )

###########OLD STUFF

french.genos<-replicate(1000,{
	sapply(french.freqs$FRQ,function(p){ rbinom(n=1,size=2,prob=p)})
})
french.num.height.alleles<-colSums(french.genos)

french.heights <- colSums ( french.genos * height.effects$EFF )

hist(french.num.height.alleles)
plot (french.num.height.alleles, french.heights*7,  pch = 20 )



sardinian.genos<-replicate(1000,{
	sapply(sardinian.freqs$FRQ,function(p){ rbinom(n=1,size=2,prob=p)})
})
sardinian.num.height.alleles <- colSums ( sardinian.genos )

layout(1:2)
hist(french.num.height.alleles, col = rgb ( 1 , 0 , 0 , 0.4 ) , breaks = 50 , xlim = c ( min (sardinian.num.height.alleles ) - 5 , max ( french.num.height.alleles ) + 5 ) , ylim = c ( 0 , 60 ) , xlab = "Number of  Tall Alleles"  , main = "")
hist(sardinian.num.height.alleles , col = rgb ( 0 , 0 , 1 , 0.4 ) , add = T , breaks = 50 )
legend ( "topright" , legend = c ( "N Europe" , "S Europe" ) , pch = 15 , col = c ( rgb ( 1 , 0 , 0 , 0.4 )  , rgb ( 0 , 0 , 1 , 0.4 ) ) , bty = "n" , cex = 1.5 )

#dev.copy2pdf(file="~/Dropbox/Courses/Height/Sardina_vs_French_num_tall_alleles.pdf")

sardinian.heights <- colSums ( sardinian.genos * height.effects$EFF )

plot (  sardinian.num.height.alleles ,sardinian.heights*7 - mean(sardinian.heights)*7, pch = 20 , col = adjustcolor("blue",0.2) , xlab = "Number of Tall Alleles" , bty = "n" ,ylim=c(-5,8),xlim=c(130,195),ylab="Predicted genetic height") #, xlim = c ( 4.9 , 7 ) )
points ( french.num.height.alleles,  french.heights *7- mean(sardinian.heights)*7, pch = 20 , col = adjustcolor("red",0.2) )
legend ( "bottomright" , legend = c ( "N Europe" , "S Europe" ) , pch = 20 , col = c ( rgb ( 1 , 0 , 0 , 0.4 )  , rgb ( 0 , 0 , 1 , 0.4 ) ) , bty = "n" , cex = 1.5)

dev.copy2pdf(file=paste(direct,"Sardina_vs_French_num_tall_alleles.pdf",sep=""))

plot(0:1,0:1,type="n")
###CODE FOR EMILY Josephs fig for review
sapply(1:length(french.freqs$FRQ),function(i){
	lines(c(0.25,.75),c(french.freqs$FRQ[i],sardinian.freqs$FRQ[i]),col=adjustcolor(ifelse(french.freqs$FRQ[i]-sardinian.freqs$FRQ[i]>0,"blue","red"),0.2))
	})


points(rep(0.75,length(french.freqs$FRQ)),sardinian.freqs$FRQ, col=adjustcolor("black",0.3))
points(rep(0.25,length(french.freqs$FRQ)),french.freqs$FRQ, col=adjustcolor("black",0.3))

points(0.25,mean(french.freqs$FRQ),col="red",pch=19)
points(0.75,mean(sardinian.freqs$FRQ),col="red",pch=19)








