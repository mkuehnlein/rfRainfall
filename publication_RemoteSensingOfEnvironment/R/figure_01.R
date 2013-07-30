#####################################
#### tune ntree ##############
#####################################

# Clear workspace
rm(list=ls(all=TRUE))

## SET WORKING DIRECTORY
path.wd <- ("/media/PRECI/rainrate_retrieval/sandbox/validation/figures/figure_tune_ntree/")
setwd(path.wd)

file.in.D = "/media/PRECI/rainrate_retrieval/sandbox/validation/pub02/tuning/ntree/day/v03/convective_ntree1000_mtry1_12_msemean.dat"
file.in.I = "/media/PRECI/rainrate_retrieval/sandbox/validation/pub02/tuning/ntree/inb/v03/convective_ntree1000_mtry1_12_msemean.dat"
file.in.N = "/media/PRECI/rainrate_retrieval/sandbox/validation/pub02/tuning/ntree/night/v03/convective_ntree1000_mtry1_12_msemean.dat"

path.out = "/media/PRECI/rainrate_retrieval/sandbox/validation/figures/figure_tune_ntree/"

# LOAD LIBRARIES
library(latticeExtra)


# READ DATA
oob.D <- read.table(file.in.D,header=F,row.names=NULL)
oob.I <- read.table(file.in.I,header=F,row.names=NULL)
oob.N <- read.table(file.in.N,header=F,row.names=NULL)
str(oob.D)
names(oob.D)
nrow(oob.D)


## plot
png(filename="ntree_convective_v03.png",width=1600,height=1600,units='px',res=300,pointsize=10)
#png(filename="ntree_stratiform_v04.png",width=1600,height=1600,units='px',res=300,pointsize=10)


plot(oob.D$V1,
     oob.D$V7,
      lwd=2, 
      col="grey80", 
      lty=1,
      xlim=c(1,1000), 
      ylim=c(2,14),
      xlab="mtry",
      ylab="MSE",
      cex.lab=1.3,
      main="",
      log = "x",
      #yaxt="n",
      #xaxt="n", 
     type="l")


#colors()
lines(oob.D$V2, lty=5, lwd=1.2, col="grey80")
lines(oob.D$V13, lty=3, lwd=1.2, col="grey80")

lines(oob.I$V7, lty=1, lwd=1.2, col="grey40")
lines(oob.I$V2, lty=5, lwd=1.2, col="grey40")
lines(oob.I$V13, lty=3, lwd=1.2, col="grey40")

lines(oob.N$V7, lty=1, lwd=1.2, col="black")
lines(oob.N$V2, lty=5, lwd=1.2, col="black")
lines(oob.N$V13, lty=3, lwd=1.2, col="black")

grid(col="gray50",lty=2,lwd=0.5)

legend("topright", 
       inset=.05, 
       c("day/mtry=1","day/mtry=6","day/mtry=15","twilight/mtry=1","twilight/mtry=6","twilight/mtry=12","night/mtry=1","night/mtry=6","night/mtry=12"),
       lty = c(5,1,3,5,1,3,5,1,3), 
       col= c("grey80","grey80","grey80","grey40","grey40","grey40","black","black","black"),
       horiz=FALSE,
       cex=0.8)
mtext(" a)", side=2,line=3, las = 1, cex=1.5, at=c(14.3),adj=1)

dev.off()




## plot stratiform
png(filename="ntree_stratiform_v05.png",width=1600,height=1600,units='px',res=300,pointsize=10)
#png(filename="ntree_stratiform_v04.png",width=1600,height=1600,units='px',res=300,pointsize=10)


plot(oob.D$V1,
     oob.D$V7,
     lwd=2, 
     col="grey80", 
     lty=1,
     xlim=c(1,1000), 
     ylim=c(0.1,0.5),
     xlab="mtry",
     ylab="MSE",
     cex.lab=1.3,
     main="",
     log = "x",
     #yaxt="n",
     #xaxt="n", 
     type="l")


#colors()
lines(oob.D$V2, lty=5, lwd=1.2, col="grey80")
lines(oob.D$V13, lty=3, lwd=1.2, col="grey80")

lines(oob.I$V7, lty=1, lwd=1.2, col="grey40")
lines(oob.I$V2, lty=5, lwd=1.2, col="grey40")
lines(oob.I$V13, lty=3, lwd=1.2, col="grey40")

lines(oob.N$V7, lty=1, lwd=1.2, col="black")
lines(oob.N$V2, lty=5, lwd=1.2, col="black")
lines(oob.N$V13, lty=3, lwd=1.2, col="black")

grid(col="gray50",lty=2,lwd=0.5)

legend("topright", 
       inset=.05, 
       c("day/mtry=1","day/mtry=6","day/mtry=15","twilight/mtry=1","twilight/mtry=6","twilight/mtry=12","night/mtry=1","night/mtry=6","night/mtry=12"),
       lty = c(5,1,3,5,1,3,5,1,3), 
       col= c("grey80","grey80","grey80","grey40","grey40","grey40","black","black","black"),
       horiz=FALSE,
       cex=0.8)
mtext(" b)", side=2,line=3, las = 1, cex=1.5, at=c(0.51),adj=1)

dev.off()
       