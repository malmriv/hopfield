#TO BE SET BY THE USER: N (number of nodes per side), frames to generate,
#title of graph and color for each neuron state.
N = 32
frames = 15
title = "Hopfield network: pattern recognition."
color1 = "black"
color0 = "white"

#FROM HERE ON, THE SCRIPT DOES NOT NEED INTERACTION.

#Read the data
s = read.table("../results.txt")$V1

#Compute number of Monte Carlo steps
steps = length(s)/(N^2)

#Set node size (for a 500x500 px .png file)
nodesize = 57/N

#Create position vectors
x = y = c(1:N)

#Create a directory to save the frames
dir.create("../frames")

for(i in 1:steps) {
  if(i%%round(steps/frames,0) == 0) {
  #Create a new .PNG file
  png(paste("../frames/",i,".png",sep=""),height=500,width=500)
  
  #Create a plot and set desired graphics
  par(cex.lab=1.3,cex.axis=1.3,cex.main=1.6,xpd=NA)
  plot(0,0,type="n",xlim=c(0,N),ylim=c(0,N),asp=1,xlab="x",ylab="y",main=title)
  box(col="black")
  for(j in 1:N) {
    for(k in 1:N) {
      if(s[(i-1)*N^2+(j-1)*N+k] == 1) lines(y[k],-x[j]+N,type="p",pch=15,col=color1,cex=nodesize)
      if(s[(i-1)*N^2+(j-1)*N+k] == 0) lines(y[k],-x[j]+N,type="p",pch=15,col=color0,cex=nodesize)
    }
  }
  dev.off()
  }
}