nswitch = function(a) {
  if(a==1) a=0
  else if(a==0) a=1
}

dir.create("../random")
files = list.files("../sets")

for(l in 1:200) {
data = read.table(paste("../sets/",files[1],sep=""))
noise = 0.5 #Completely random
random = runif(length(data[,1]),min=0,max=1)
  for(i in 1:length(data[,1])) {
    if(random[i]<noise) {
      data[i,1] = nswitch(data[i,1])
    }
  }
  write.table(data,file=paste("../random/",l,".txt",sep=""),col.names=FALSE,row.names=FALSE,quote=FALSE)
}