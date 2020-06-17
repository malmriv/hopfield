net=32^2
divs=512

avalue = seq(0.01,0.99,len=divs)
pattern = seq(0,0,len=net)
meanweight = seq(0,0,len=divs)

for(i in 1:divs) {
  weights = matrix(data=0,ncol=net,nrow=net)
  for(j in 1:net) if(runif(1)<avalue[i]) pattern[j] = 1
  for(l in 1:net) {
    for(m in 1:net) {
      weights[l,m] = 1/(avalue[i]*(1-avalue[i])*net)*(pattern[l]-avalue[i])*(pattern[m]-avalue[i])
    }
  }
  meanweight[i] = mean(abs(weights))
}
polinom = loess(meanweight ~ avalue, span=0.1)
interpol = splinefun(meanweight ~ avalue)
plot(avalue,meanweight,type="n",xlab=TeX("$< \\xi >$"),ylab=TeX("$<\\omega_{ij}>$"),
     main="Average weight vs. average pattern bit.")
mtext("N = 1024 neurons, 512 simulated patterns.",side=3,line=0.3,cex=0.95)
lines(seq(0,1,len=1000),predict(polinom,seq(0,1,len=1000)),col="black",lwd=1.5)
abline(h=max(meanweight),lty=2,lwd=1,col="grey")

