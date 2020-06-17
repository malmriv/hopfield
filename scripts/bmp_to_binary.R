library(bmp)
library(pixmap)
side = 32
files = list.files("../patterns")

for(k in 1:length(files)) {
  var = read.bmp(paste("../patterns/",files[k],sep=""))
  var = as.matrix(var[,,3]) #Blue channel
  var = round(var/255,0) #Turn into 0 or 1 (threshold: >128 = 1)
  var = abs(var-1)
  split = strsplit(files[k],split=NULL)
  filename = paste("../sets/",sub(".bmp",".txt",files[k]),sep="")
  for(i in 1:side) {
    for(j in 1:side) {
      write.table(var[i,j],filename,col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
    }
  }
}
