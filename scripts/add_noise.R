nswitch = function(a) {
  if(a==1) a=0
  else if(a==0) a=1
}

files = list.files("../sets")
for(l in 1:length(files)) {
data = read.table(paste("../sets/",files[l],sep=""))
noise = 0.25
random = runif(length(data[,1]),min=0,max=1)
  for(i in 1:length(data[,1])) {
    if(random[i]<noise) {
      data[i,1] = nswitch(data[i,1])
    }
  }
  write.table(data,file=paste("../sets/",sub(".txt","",files[l]),"c.txt",sep=""),
              col.names=FALSE,row.names=FALSE,quote=FALSE)
}