#Import data
montecarlo = seq(0,10,length=10240)
overlap=read.table("overlap.txt")

#Make basic plot
par(mar=c(4,4,4,4))
plot(montecarlo,overlap[1,],type="l",ylim=c(min(overlap),max(overlap)),
     main="Overlap parameter for different temperatures.",
     xlab="Monte Carlo iterations (adim.)", ylab="overlap (adim.)",
     col=plasma(50,alpha=0.9)[1], lwd = 0.9)

#Write the rest of the data
for(i in 2:50) {
  lines(montecarlo,overlap[i,],type="l",col=plasma(50,alpha=0.9)[i],
        lwd = 0.9)
}

#Add some other things
mtext("P = 10, initial corruption: 20%",side=3,
      line=0.4, cex=0.9)
gradientLegend(valRange=c(0,0.3),color=plasma(50,alpha=0.9),
               nCol=50,side=4,length=0.5,depth=0.05,
               dec=2,inside=FALSE,border.col="black",tick.col=NA,
               fit.margin=TRUE, coords=TRUE,pos=c(10.65,0.2,11.25,0.9))

#Another way of visualising this: final values
final = seq(0,0,len=50)
for(i in 1:50) final[i] = overlap[i,10240]
plot(seq(0,0.3,len=50),seq(0,1,len=50),type="n",xlab="temperature (K)",
     ylab="overlap (adim.)",main="Final overlap as function of temperature.")
for(i in 1:50) lines(0.006*i,final[i],type="p",col=plasma(50,alpha=0.9)[i],
                     pch=19)
gradientLegend(valRange=c(0,0.3),color=plasma(50,alpha=0.9),
               nCol=50,side=4,length=0.5,depth=0.05,
               dec=2,inside=FALSE,border.col="black",tick.col=NA,
               fit.margin=TRUE, coords=TRUE,pos=c(0.32,0.2,0.335,0.9))
model = lm(final ~ seq(0,0.3,len=50))
abline(model,lty=2,lwd=1.5,col="black")
legend(0.2,1,legend=c("data","least-squares fit"),cex=0.8,pch=c(19,NA),
       lty=c(NA,2),lwd=c(NA,2),col=c(plasma(50)[25],"black"))
