data fromhttps://sci-hub.tw/https://pediatrics.aappublications.org/content/104/1/61.figures-only?sso=1&sso_redirect_count=1&nfstatus=401&nftoken=00000000-0000-0000-0000-000000000000&nfstatusdescription=ERROR%3a+No+local+token

twins<-read.csv("/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/twins_body_fat/twins_body_fat.csv")

plot(twins$MZ_twin_1,twins$MZ_twin_2,cex=1.5,col="black",pch=19,
     xlab="Twin 1 (PBF)",ylab="Twin 2 (PBF)",cex.lab=1.4,cex.axis=1.2,xlim=c(0,70),ylim=c(0,70))
abline(lm(twins$MZ_twin_2~ twins$MZ_twin_1),lwd=2)

points(twins$DZ_twin_1,twins$DZ_twin_2,cex=1.5)
abline(lm(twins$DZ_twin_2~ twins$DZ_twin_1),lty=2,lwd=2)
legend(x="topleft",legend=c("MZ","DZ"),pch=c(19,1),cex=1.2)
dev.copy2pdf(file="/Users/gcoop/Dropbox/Courses/Popgen_teaching_Notes/Journal_figs/Quant_gen/twins_body_fat/twins_body_fat.pdf")
