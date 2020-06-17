e1 = read.table("energy1.txt")$V1
e2 = read.table("energy2.txt")$V1
e3 = read.table("energy3.txt")$V1
e4 = read.table("energy4.txt")$V1
e5 = read.table("energy5.txt")$V1
e6 = read.table("energy6.txt")$V1
e7 = read.table("energy7.txt")$V1
e8 = read.table("energy8.txt")$V1
e9 = read.table("energy9.txt")$V1
e10 = read.table("energy10.txt")$V1

e1acum = seq(0,0,len=length(e1)+1)
e2acum = seq(0,0,len=length(e1)+1)
e3acum = seq(0,0,len=length(e1)+1)
e4acum = seq(0,0,len=length(e1)+1)
e5acum = seq(0,0,len=length(e1)+1)
e6acum = seq(0,0,len=length(e1)+1)
e7acum = seq(0,0,len=length(e1)+1)
e8acum = seq(0,0,len=length(e1)+1)
e9acum = seq(0,0,len=length(e1)+1)
e10acum = seq(0,0,len=length(e1)+1)

for(i in 2:length(e1)+1) {
  e1acum[i] = e1acum[i-1] + e1[i-1]
  e2acum[i] = e2acum[i-1] + e2[i-1]
  e3acum[i] = e3acum[i-1] + e3[i-1]
  e4acum[i] = e4acum[i-1] + e4[i-1]
  e5acum[i] = e5acum[i-1] + e5[i-1]
  e6acum[i] = e6acum[i-1] + e6[i-1]
  e7acum[i] = e7acum[i-1] + e7[i-1]
  e8acum[i] = e8acum[i-1] + e8[i-1]
  e9acum[i] = e9acum[i-1] + e9[i-1]
  e10acum[i] = e10acum[i-1] + e10[i-1]
}

library(latex2exp)
steps = seq(1,length(e1),len=length(e1)+1)
plot(steps,e1acum,type="l",xlim=c(0,32^2*10),col=rainbow(10)[1],
     main="Energy evolution during pattern retrieval.",
     xlab="iterations",ylab=latex2exp("$E(s) - E(s_o)$"),
     lty=1,lwd=2,ylim=c(-65,0))

lines(steps,e2acum,type="l",col=rainbow(10)[2],lwd=2)
lines(steps,e3acum,type="l",col=rainbow(10)[3],lwd=2)
lines(steps,e4acum,type="l",col=rainbow(10)[4],lwd=2)
lines(steps,e5acum,type="l",col=rainbow(10)[5],lwd=2)
lines(steps,e6acum,type="l",col=rainbow(10)[6],lwd=2)
lines(steps,e7acum,type="l",col=rainbow(10)[7],lwd=2)
lines(steps,e8acum,type="l",col=rainbow(10)[8],lwd=2)
lines(steps,e9acum,type="l",col=rainbow(10)[9],lwd=2)
lines(steps,e10acum,type="l",col=rainbow(10)[10],lwd=2)

for(i in 1:15) abline(v = 32^2*i, lty=2, col="lightblue")
legend(8000,0,paste("pattern no. ",c(1:10),sep=""),lty=1,
       col=rainbow(10)[1:10],cex=0.7)
mtext(side=3,line=0.5,"Ten different patterns, 20% data corruption.", cex=0.8)
