E = seq(-10,10,len=2000)
beta = seq(0.1,1,len=10)
par(mar=c(4,4,4,4.3))
plot(0,0,xlim=c(-2,10),ylim=c(0,1),type="n",xlab="ΔE",ylab="T(X'-X)",
     main="Equation proposed by Metropolis et al. (1953)")
mtext(side=3,line=0.4,"Different β values in the range [0.1 - 1]",
      cex=0.9)

for(i in 1:15) {
p = pmin(1,exp(-beta[i]*E))
lines(E,p,type="l",col=rainbow(15)[i],lty=1,lwd=2)
}

mtext(side=4,line=-0.2,"β",cex=0.7)
library(plotfunctions)
library(viridis)
gradientLegend(valRange=c(0.1,1),color=rainbow(15,alpha=0.1)[1:11],
               nCol=100,side=4, dec=2,inside=FALSE, border.col="black",
               tick.col=NA, fit.margin=TRUE, coords=TRUE,pos=c(11,-0.04,11.4,1.05))
