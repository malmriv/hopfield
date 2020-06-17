#Randomise file to read
corrupted = c("./sets/pattern1-corrupted.txt","./sets/pattern2-corrupted.txt",
           "./sets/pattern3-corrupted.txt","./sets/pattern4-corrupted.txt",
           "./sets/pattern5-corrupted.txt","./sets/pattern6-corrupted.txt",
           "./sets/pattern7-corrupted.txt","./sets/pattern8-corrupted.txt",
           "./sets/pattern9-corrupted.txt","./sets/pattern10-corrupted.txt")

right = 0
total = 50

for(k in 1:total) {
chosen = round(runif(1,min=1,max=10),0)
write.table(corrupted[chosen],file="./enigma.txt",
            col.names=FALSE,row.names=FALSE,quote=FALSE)
system("./sim")

#Execute the program
overlap = read.table("./overlap.txt")
mc = seq(1,length(overlap[,1]),len=length(overlap[,1]))
par(xpd=FALSE)
plot(0,0,type="n",ylim=c(min(overlap),1),
     xlim=c(1,length(overlap[,1])),main="Pattern recognition: overlap parameter.",
     xlab="Monte Carlo iterations (adim.)",ylab="overlap (adim.)")
candidate = as.numeric(which(overlap[length(overlap[,1]),]==max(overlap[length(overlap[,1]),]),
                  arr.ind=TRUE)[1,2])

for(i in 1:length(overlap[,1])) {
  lines(mc,overlap[,i],type="l",col=rainbow(length(overlap[1,]))[i])
}

#I did NOT write this.Source:
# https://stackoverflow.com/questions/27800307/adding-a-picture-to-plot-in-r
addImg <- function(
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing. 
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj, 
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), 
              interpolate = interpolate)
}

#Add the recognised image
par(xpd=NA)
library(bmp)
thumbnail = read.bmp(paste("./patterns/pattern",candidate,".bmp",sep=""))
frame = read.bmp("./scripts/frame.bmp")
thumbnail = thumbnail/255
frame = frame/255
addImg(frame, x = 0.2*length(overlap[,1]), y = 0.85, width = 1)
addImg(thumbnail, x = 0.2*length(overlap[,1]), y = 0.85, width = 0.95)
text(x = 0.2*length(overlap[,1])+1.5, y = 0.85,
     label=paste("Best candidate: \n pattern",candidate,".bmp",sep=""),cex=0.8)


#Keep count of successful recalls
if(candidate == chosen) right = right+1
}
