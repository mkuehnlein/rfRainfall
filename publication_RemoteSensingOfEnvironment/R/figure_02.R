#####################################
#### bwplot ##############
#####################################

# Clear workspace
rm(list=ls(all=TRUE))

## SET WORKING DIRECTORY
path.wd <- ("/media/windows/tappelhans/uni/marburg/colleagues/meike/randomforest/boxplots/figure_2/")
setwd(path.wd)
path.in = "/media/windows/tappelhans/uni/marburg/colleagues/meike/randomforest/boxplots/figure_2/"
version <- "v03"

## LOAD LIBRARY
#library(latticeExtra)
library(grid)
library(ggplot2)

## DAY CONVECTIVE ########################################################################################
## READ DATA
#oob.mtry <- read.table(paste(paste(paste(paste(path.in,"/day/",sep=""),version,sep=""),"/",sep=""),"convective_ntree500_mtry_mse500.dat",sep=""),header=F,row.names=NULL)
oob.mtry <- read.table("convective_ntree500_mtry_mse500.dat", header = F,
                       row.names = NULL)


oob.rf1 <- oob.mtry[,!(names(oob.mtry) %in% "V1")]
obb.rf <- data.frame(matrix(unlist(unclass(oob.rf1)),nrow=length(oob.rf1),byrow=T,dimnames=list(names(oob.rf1),oob.rf1[,0])))
obb.rf.clean <- lapply(seq(ncol(obb.rf)), function(i) {
  obb.rf[, i]
})
obb.rf.clean <- unlist(obb.rf.clean)

names.vec <- 1:length(oob.rf1)
names.vec <- rep(names.vec,each=nrow(oob.mtry))
names.vec <- paste("mtry",names.vec,sep="")

oob.df <- data.frame(names.vec,obb.rf.clean)

print(levels(oob.df$names.vec))
oob.df$names.vec <- factor(oob.df$names.vec, 
                           levels = c("mtry1", "mtry2", "mtry3", "mtry4","mtry5", "mtry6", "mtry7", "mtry8", "mtry9", "mtry10", "mtry11", "mtry12"))
oob.df$process <- "DAY-C"

## DAY STRATIFORM ########################################################################################
## READ DATA
#oob.mtry <- read.table(paste(paste(paste(paste(path.in,"/day/",sep=""),version,sep=""),"/",sep=""),"stratiform_ntree500_mtry_mse500.dat",sep=""),header=F,row.names=NULL)
oob.mtry.s <- read.table("stratiform_ntree500_mtry_mse500.dat", header = F,
                         row.names = NULL)

oob.rf1.s <- oob.mtry.s[,!(names(oob.mtry.s) %in% "V1")]
obb.rf.s <- data.frame(matrix(unlist(unclass(oob.rf1.s)),nrow=length(oob.rf1.s),byrow=T,dimnames=list(names(oob.rf1.s),oob.rf1.s[,0])))
obb.rf.clean.s <- lapply(seq(ncol(obb.rf.s)), function(i) {
  obb.rf.s[, i]
})
obb.rf.clean.s <- unlist(obb.rf.clean.s)

names.vec.s <- 1:length(oob.rf1.s)
names.vec.s <- rep(names.vec.s,each=nrow(oob.mtry.s))
names.vec.s <- paste("mtry",names.vec.s,sep="")

oob.df.s <- data.frame(names.vec.s,obb.rf.clean.s)

print(levels(oob.df.s$names.vec.s))
oob.df.s$names.vec.s <- factor(oob.df.s$names.vec.s, 
                           levels = c("mtry1", "mtry2", "mtry3", "mtry4","mtry5", "mtry6", "mtry7", "mtry8", "mtry9", "mtry10", "mtry11", "mtry12"))
oob.df.s$process <- "DAY-S"


### Tim's ggplot2 version
names(oob.df.s) <- names(oob.df)
oob.df.both <- rbind(oob.df, oob.df.s)

oob.mtry.p <- ggplot(oob.df.both, aes(x = names.vec, y = obb.rf.clean))

png("figure_bwplot_mtry_day.png", width=3000,height=2000, units='px',res=300)
oob.mtry.p +
  geom_boxplot(fill = "grey80") +
  facet_wrap( ~ process, ncol = 1, scales = "free_y") +
  theme_bw() +
  xlab("") +
  ylab("MSE")
dev.off()

# ### Meike's lattice version
# png(filename=paste(paste("figure_bwplot_mtry_day_convective_",version,sep=""),".png",sep=""), width=3000,height=2000, units='px',res=300)
# bwplot(oob.df$obb.rf.clean ~ oob.df$names.vec,
#        varwidth = TRUE,
#        #ylim=c(3,11),
#        ylim=c(4,20),
#        #cales = list(x = list(labels = c("","","","","","","","","","","","",""), 
#        #                        alternating = 1, tck = c(1, 0))),
#        pch = "|", par.settings = list(
#          plot.symbol = list(pch = "*", col = "black"),
#          box.umbrella = list(lty = 1, col = "grey40"),
#          box.rectangle = list(fill = "grey80", col = "black")),
#        ylab="MSE",
#        xlab="",
#        label=c(""),
#        #xlab="mtry",
#        #scales=list(x=list(rot=45)),
#        asp=0.3,
#        page = function(page) grid.text('a)', x = 0.015, y = 0.85),
#        panel = function(...) {
#          panel.bwplot(...)
#          panel.grid(h = -1, v = -1, col="gray50",lty=2)
#          draw.key(list(text=list(c("DAY-C"), cex = 0.9),
#                        columns = 1, rows = 1,
#                        #points = list(pch = c(18, 17, 15),
#                        #               cex = 1.2),
#                        #col = c(brewer.pal(6, "Greys")[5],brewer.pal(6, "Greys")[4],brewer.pal(6, "Greys")[3])),
#                        background = "white", border = T,
#                        padding.text = 2),
#                   draw = T,
#                   vp = viewport(x = unit(0.93, "npc"),
#                                 y = unit(0.9, "npc"),
#                                 just = "centre"))})
# dev.off()

# ## DAY STRATIFORM ########################################################################################
# ## READ DATA
# oob.mtry <- read.table(paste(paste(paste(paste(path.in,"/day/",sep=""),version,sep=""),"/",sep=""),"stratiform_ntree500_mtry_mse500.dat",sep=""),header=F,row.names=NULL)
# 
# oob.rf1 <- oob.mtry[,!(names(oob.mtry) %in% "V1")]
# obb.rf <- data.frame(matrix(unlist(unclass(oob.rf1)),nrow=length(oob.rf1),byrow=T,dimnames=list(names(oob.rf1),oob.rf1[,0])))
# obb.rf.clean <- lapply(seq(ncol(obb.rf)), function(i) {
#   obb.rf[, i]
# })
# obb.rf.clean <- unlist(obb.rf.clean)
# 
# names.vec <- 1:length(oob.rf1)
# names.vec <- rep(names.vec,each=nrow(oob.mtry))
# names.vec <- paste("mtry",names.vec,sep="")
# 
# oob.df <- data.frame(names.vec,obb.rf.clean)
# 
# print(levels(oob.df$names.vec))
# oob.df$names.vec <- factor(oob.df$names.vec, 
#                            levels = c("mtry1", "mtry2", "mtry3", "mtry4","mtry5", "mtry6", "mtry7", "mtry8", "mtry9", "mtry10", "mtry11", "mtry12"))
# 

# ### Meike's lattice version
# png(filename=paste(paste("figure_bwplot_mtry_day_stratiform_",version,sep=""),".png",sep=""), width=3000,height=2000, units='px',res=300)
# bwplot(oob.df$obb.rf.clean ~ oob.df$names.vec,
#        varwidth = TRUE,
#        #ylim=c(3,11),
#        ylim=c(0.2,0.23),
#        #cales = list(x = list(labels = c("","","","","","","","","","","","",""), 
#        #                        alternating = 1, tck = c(1, 0))),
#        pch = "|", par.settings = list(
#          plot.symbol = list(pch = "*", col = "black"),
#          box.umbrella = list(lty = 1, col = "grey40"),
#          box.rectangle = list(fill = "grey80", col = "black")),
#        ylab="MSE",
#        xlab="",
#        label=c(""),
#        #xlab="mtry",
#        #scales=list(x=list(rot=45)),
#        asp=0.3,
#        page = function(page) grid.text('b)', x = 0.015, y = 0.85),
#        panel = function(...) {
#          panel.bwplot(...)
#          panel.grid(h = -1, v = -1, col="gray50",lty=2)
#          draw.key(list(text=list(c("DAY-S"), cex = 0.9),
#                        columns = 1, rows = 1,
#                        #points = list(pch = c(18, 17, 15),
#                        #               cex = 1.2),
#                        #col = c(brewer.pal(6, "Greys")[5],brewer.pal(6, "Greys")[4],brewer.pal(6, "Greys")[3])),
#                        background = "white", border = T,
#                        padding.text = 2),
#                   draw = T,
#                   vp = viewport(x = unit(0.93, "npc"),
#                                 y = unit(0.9, "npc"),
#                                 just = "centre"))})
# dev.off()


# ## inb CONVECTIVE ########################################################################################
# ## READ DATA
# oob.mtry <- read.table(paste(paste(paste(paste(path.in,"/inb/",sep=""),version,sep=""),"/",sep=""),"convective_ntree500_mtry_mse500.dat",sep=""),header=F,row.names=NULL)
# 
# oob.rf1 <- oob.mtry[,!(names(oob.mtry) %in% "V1")]
# obb.rf <- data.frame(matrix(unlist(unclass(oob.rf1)),nrow=length(oob.rf1),byrow=T,dimnames=list(names(oob.rf1),oob.rf1[,0])))
# obb.rf.clean <- lapply(seq(ncol(obb.rf)), function(i) {
#   obb.rf[, i]
# })
# obb.rf.clean <- unlist(obb.rf.clean)
# 
# names.vec <- 1:length(oob.rf1)
# names.vec <- rep(names.vec,each=nrow(oob.mtry))
# names.vec <- paste("mtry",names.vec,sep="")
# 
# oob.df <- data.frame(names.vec,obb.rf.clean)
# 
# print(levels(oob.df$names.vec))
# oob.df$names.vec <- factor(oob.df$names.vec, 
#                            levels = c("mtry1", "mtry2", "mtry3", "mtry4","mtry5", "mtry6", "mtry7", "mtry8", "mtry9", "mtry10", "mtry11", "mtry12"))
# 
# png(filename=paste(paste("figure_bwplot_mtry_inb_convective_",version,sep=""),".png",sep=""), width=3000,height=2000, units='px',res=300)
# bwplot(oob.df$obb.rf.clean ~ oob.df$names.vec,
#        varwidth = TRUE,
#        #ylim=c(3,11),
#        ylim=c(4,20),
#        #cales = list(x = list(labels = c("","","","","","","","","","","","",""), 
#        #                        alternating = 1, tck = c(1, 0))),
#        pch = "|", par.settings = list(
#          plot.symbol = list(pch = "*", col = "black"),
#          box.umbrella = list(lty = 1, col = "grey40"),
#          box.rectangle = list(fill = "grey80", col = "black")),
#        ylab="MSE",
#        xlab="",
#        label=c(""),
#        #xlab="mtry",
#        #scales=list(x=list(rot=45)),
#        asp=0.3,
#        page = function(page) grid.text('c)', x = 0.015, y = 0.85),
#        panel = function(...) {
#          panel.bwplot(...)
#          panel.grid(h = -1, v = -1, col="gray50",lty=2)
#          draw.key(list(text=list(c("TWILIGHT-C"), cex = 0.9),
#                        columns = 1, rows = 1,
#                        #points = list(pch = c(18, 17, 15),
#                        #               cex = 1.2),
#                        #col = c(brewer.pal(6, "Greys")[5],brewer.pal(6, "Greys")[4],brewer.pal(6, "Greys")[3])),
#                        background = "white", border = T,
#                        padding.text = 2),
#                   draw = T,
#                   vp = viewport(x = unit(0.93, "npc"),
#                                 y = unit(0.9, "npc"),
#                                 just = "centre"))})
# dev.off()
# 
# ## inb STRATIFORM ########################################################################################
# ## READ DATA
# oob.mtry <- read.table(paste(paste(paste(paste(path.in,"/inb/",sep=""),version,sep=""),"/",sep=""),"stratiform_ntree500_mtry_mse500.dat",sep=""),header=F,row.names=NULL)
# 
# oob.rf1 <- oob.mtry[,!(names(oob.mtry) %in% "V1")]
# obb.rf <- data.frame(matrix(unlist(unclass(oob.rf1)),nrow=length(oob.rf1),byrow=T,dimnames=list(names(oob.rf1),oob.rf1[,0])))
# obb.rf.clean <- lapply(seq(ncol(obb.rf)), function(i) {
#   obb.rf[, i]
# })
# obb.rf.clean <- unlist(obb.rf.clean)
# 
# names.vec <- 1:length(oob.rf1)
# names.vec <- rep(names.vec,each=nrow(oob.mtry))
# names.vec <- paste("mtry",names.vec,sep="")
# 
# oob.df <- data.frame(names.vec,obb.rf.clean)
# 
# print(levels(oob.df$names.vec))
# oob.df$names.vec <- factor(oob.df$names.vec, 
#                            levels = c("mtry1", "mtry2", "mtry3", "mtry4","mtry5", "mtry6", "mtry7", "mtry8", "mtry9", "mtry10", "mtry11", "mtry12"))
# 
# png(filename=paste(paste("figure_bwplot_mtry_inb_stratiform_",version,sep=""),".png",sep=""), width=3000,height=2000, units='px',res=300)
# bwplot(oob.df$obb.rf.clean ~ oob.df$names.vec,
#        varwidth = TRUE,
#        #ylim=c(3,11),
#        ylim=c(0.2,0.23),
#        #cales = list(x = list(labels = c("","","","","","","","","","","","",""), 
#        #                        alternating = 1, tck = c(1, 0))),
#        pch = "|", par.settings = list(
#          plot.symbol = list(pch = "*", col = "black"),
#          box.umbrella = list(lty = 1, col = "grey40"),
#          box.rectangle = list(fill = "grey80", col = "black")),
#        ylab="MSE",
#        xlab="",
#        label=c(""),
#        #xlab="mtry",
#        #scales=list(x=list(rot=45)),
#        asp=0.3,
#        page = function(page) grid.text('d)', x = 0.015, y = 0.85),
#        panel = function(...) {
#          panel.bwplot(...)
#          panel.grid(h = -1, v = -1, col="gray50",lty=2)
#          draw.key(list(text=list(c("TWILIGHT-S"), cex = 0.9),
#                        columns = 1, rows = 1,
#                        #points = list(pch = c(18, 17, 15),
#                        #               cex = 1.2),
#                        #col = c(brewer.pal(6, "Greys")[5],brewer.pal(6, "Greys")[4],brewer.pal(6, "Greys")[3])),
#                        background = "white", border = T,
#                        padding.text = 2),
#                   draw = T,
#                   vp = viewport(x = unit(0.93, "npc"),
#                                 y = unit(0.9, "npc"),
#                                 just = "centre"))})
# dev.off()
# 
# 
# ## night CONVECTIVE ########################################################################################
# ## READ DATA
# oob.mtry <- read.table(paste(paste(paste(paste(path.in,"night/",sep=""),version,sep=""),"/",sep=""),"convective_ntree500_mtry_mse500.dat",sep=""),header=F,row.names=NULL)
# 
# oob.rf1 <- oob.mtry[,!(names(oob.mtry) %in% "V1")]
# obb.rf <- data.frame(matrix(unlist(unclass(oob.rf1)),nrow=length(oob.rf1),byrow=T,dimnames=list(names(oob.rf1),oob.rf1[,0])))
# obb.rf.clean <- lapply(seq(ncol(obb.rf)), function(i) {
#   obb.rf[, i]
# })
# obb.rf.clean <- unlist(obb.rf.clean)
# 
# names.vec <- 1:length(oob.rf1)
# names.vec <- rep(names.vec,each=nrow(oob.mtry))
# names.vec <- paste("mtry",names.vec,sep="")
# 
# oob.df <- data.frame(names.vec,obb.rf.clean)
# 
# print(levels(oob.df$names.vec))
# oob.df$names.vec <- factor(oob.df$names.vec, 
#                            levels = c("mtry1", "mtry2", "mtry3", "mtry4","mtry5", "mtry6", "mtry7", "mtry8", "mtry9", "mtry10", "mtry11", "mtry12"))
# 
# png(filename=paste(paste("figure_bwplot_mtry_night_convective_",version,sep=""),".png",sep=""), width=3000,height=2000, units='px',res=300)
# bwplot(oob.df$obb.rf.clean ~ oob.df$names.vec,
#        varwidth = TRUE,
#        #ylim=c(3,11),
#        ylim=c(4,20),
#        #cales = list(x = list(labels = c("","","","","","","","","","","","",""), 
#        #                        alternating = 1, tck = c(1, 0))),
#        pch = "|", par.settings = list(
#          plot.symbol = list(pch = "*", col = "black"),
#          box.umbrella = list(lty = 1, col = "grey40"),
#          box.rectangle = list(fill = "grey80", col = "black")),
#        ylab="MSE",
#        xlab="",
#        label=c(""),
#        #xlab="mtry",
#        #scales=list(x=list(rot=45)),
#        asp=0.3,
#        page = function(page) grid.text('e)', x = 0.015, y = 0.85),
#        panel = function(...) {
#          panel.bwplot(...)
#          panel.grid(h = -1, v = -1, col="gray50",lty=2)
#          draw.key(list(text=list(c("NIGHT-C"), cex = 0.9),
#                        columns = 1, rows = 1,
#                        #points = list(pch = c(18, 17, 15),
#                        #               cex = 1.2),
#                        #col = c(brewer.pal(6, "Greys")[5],brewer.pal(6, "Greys")[4],brewer.pal(6, "Greys")[3])),
#                        background = "white", border = T,
#                        padding.text = 2),
#                   draw = T,
#                   vp = viewport(x = unit(0.93, "npc"),
#                                 y = unit(0.9, "npc"),
#                                 just = "centre"))})
# dev.off()
# 
# ## night STRATIFORM ########################################################################################
# ## READ DATA
# oob.mtry <- read.table(paste(paste(paste(paste(path.in,"/night/",sep=""),version,sep=""),"/",sep=""),"stratiform_ntree500_mtry_mse500.dat",sep=""),header=F,row.names=NULL)
# 
# oob.rf1 <- oob.mtry[,!(names(oob.mtry) %in% "V1")]
# obb.rf <- data.frame(matrix(unlist(unclass(oob.rf1)),nrow=length(oob.rf1),byrow=T,dimnames=list(names(oob.rf1),oob.rf1[,0])))
# obb.rf.clean <- lapply(seq(ncol(obb.rf)), function(i) {
#   obb.rf[, i]
# })
# obb.rf.clean <- unlist(obb.rf.clean)
# 
# names.vec <- 1:length(oob.rf1)
# names.vec <- rep(names.vec,each=nrow(oob.mtry))
# names.vec <- paste("mtry",names.vec,sep="")
# 
# oob.df <- data.frame(names.vec,obb.rf.clean)
# 
# print(levels(oob.df$names.vec))
# oob.df$names.vec <- factor(oob.df$names.vec, 
#                            levels = c("mtry1", "mtry2", "mtry3", "mtry4","mtry5", "mtry6", "mtry7", "mtry8", "mtry9", "mtry10", "mtry11", "mtry12"))
# 
# png(filename=paste(paste("figure_bwplot_mtry_night_stratiform_",version,sep=""),".png",sep=""), width=3000,height=2000, units='px',res=300)
# bwplot(oob.df$obb.rf.clean ~ oob.df$names.vec,
#        varwidth = TRUE,
#        #ylim=c(3,11),
#        ylim=c(0.2,0.23),
#        #cales = list(x = list(labels = c("","","","","","","","","","","","",""), 
#        #                        alternating = 1, tck = c(1, 0))),
#        pch = "|", par.settings = list(
#          plot.symbol = list(pch = "*", col = "black"),
#          box.umbrella = list(lty = 1, col = "grey40"),
#          box.rectangle = list(fill = "grey80", col = "black")),
#        ylab="MSE",
#        xlab="",
#        label=c(""),
#        #xlab="mtry",
#        #scales=list(x=list(rot=45)),
#        asp=0.3,
#        page = function(page) grid.text('f)', x = 0.015, y = 0.85),
#        panel = function(...) {
#          panel.bwplot(...)
#          panel.grid(h = -1, v = -1, col="gray50",lty=2)
#          draw.key(list(text=list(c("NIGHT-S"), cex = 0.9),
#                        columns = 1, rows = 1,
#                        #points = list(pch = c(18, 17, 15),
#                        #               cex = 1.2),
#                        #col = c(brewer.pal(6, "Greys")[5],brewer.pal(6, "Greys")[4],brewer.pal(6, "Greys")[3])),
#                        background = "white", border = T,
#                        padding.text = 2),
#                   draw = T,
#                   vp = viewport(x = unit(0.93, "npc"),
#                                 y = unit(0.9, "npc"),
#                                 just = "centre"))})
# dev.off()
