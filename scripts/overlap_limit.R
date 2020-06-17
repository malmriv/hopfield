#Randomise file to read
patterns = 120
right = 0
spurious = 0
total = 30

for(k in 1:total) {

chosen = round(runif(1,min=1,max=patterns),0)
write.table(paste('./randomnoise/',chosen,'.txt',sep=""),file="./enigma.txt",
            col.names=FALSE,row.names=FALSE,quote=FALSE)

#Execute the program
system("./sim")
overlap = read.table("./overlap.txt")
mc = seq(1,length(overlap[,1]),len=length(overlap[,1]))
par(xpd=FALSE)
plot(0,0,type="n",ylim=c(min(overlap),1),
     xlim=c(1,length(overlap[,1])),main="Pattern recognition: overlap parameter.",
     xlab="Monte Carlo iterations (adim.)",ylab="overlap (adim.)")
candidate = as.numeric(which(overlap[length(overlap[,1]),]>0.75,
                             arr.ind=TRUE))

for(i in 1:length(overlap[1,])) {
  lines(mc,overlap[,i],type="l",col=rainbow(length(overlap[1,]))[i])
}

#Keep count of successful recalls
if(length(candidate) == 0) right = right
else if(length(candidate) == 2) {
  if(candidate[2] == chosen) {right = right + 1}
  else{spurious = spurious + 1}
}
else if(length(candidate) > 2) spurious = spurious +1
}