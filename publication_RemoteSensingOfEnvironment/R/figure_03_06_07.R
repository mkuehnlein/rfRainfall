library(ggplot2)
library(grid)
library(RColorBrewer)
library(plyr)

#### set global parameters #####################################################
setwd("/media/windows/tappelhans/uni/marburg/colleagues/meike/randomforest/boxplots/")
labs <- c("day", "twilight", "night")
fill.clrs <- brewer.pal(3, "Blues")

#### boxplot over scenes #######################################################

scenes_1h <- read.table("data_scenes/statistics_scenes_1h.csv",
                     header = TRUE)

scenes_1h$tres <- "01h"

scenes_3h <- read.table("data_scenes/statistics_scenes_3h.csv",
                        header = TRUE)

scenes_3h$tres <- "03h"

scenes_12h <- read.table("data_scenes/statistics_scenes_12h.csv",
                        header = TRUE)

scenes_12h$tres <- "12h"

all.scenes <- rbind(scenes_1h, scenes_3h, scenes_12h)
all.scenes$TOD <- ordered(all.scenes$TOD, levels = c("day", "twilight", "night"))

all.scns <- data.frame(values = c(all.scenes$R2, all.scenes$RMSE, 
                                   all.scenes$MAE, all.scenes$ME),
                        stat = c(rep("Rsq.", nrow(all.scenes)), 
                                 rep("RMSE", nrow(all.scenes)), 
                                 rep("MAE", nrow(all.scenes)), 
                                 rep("ME", nrow(all.scenes))), 
                        tod = all.scenes$TOD, tres = all.scenes$tres)

# all.scns.mean.R2 <- xtabs(all.scenes$R2 ~ all.scenes$TOD + all.scenes$tres) / 
#   xtabs( ~ all.scenes$TOD + all.scenes$tres)

all.scns$stat <- ordered(all.scns$stat, levels = c("Rsq.", "RMSE", "MAE", "ME"))
all.scns$tod <- ordered(all.scns$tod, levels = c("day", "twilight", "night"))

w <- sqrt(table(all.scns$tod)/nrow(all.scns))


all.scns.stats <- ggplot(all.scns, aes(tod, values)) +
  facet_grid(stat ~ tres, as.table = TRUE, scales = "free_y", 
             labeller = "label_value") +   
    llply(unique(all.scns$tod), 
          function(i) geom_boxplot(aes(fill = tod),
                                   width = w[i], outlier.shape = "*",
                                   outlier.size = 3,
                                   data = subset(all.scns, tod == i))) +
  scale_fill_manual(values = fill.clrs, name = "Time of day", 
                    limits = levels(all.scns$tod)) +
  scale_x_discrete(name = "", limits = levels(all.scns$tod)) +
  scale_y_continuous(name = "")

pdf("data_scenes/bplot_scenes.pdf", 
    height = 10, width = 8)

print(all.scns.stats)

dev.off()

png("data_scenes/bplot_scenes.png", 
    height = 1024 * 3, width = 748 * 3, res = 300)

print(all.scns.stats)

dev.off()

png("data_scenes/bplot_scenes_bw.png", 
    height = 1024 * 3, width = 748 * 3, res = 300)

print(all.scns.stats + theme_bw())

dev.off()

#### boxplot over versions #####################################################

versions_1h <- read.table("data_versions/statistics_versions_1h.csv",
                        header = TRUE)

versions_1h$tres <- "01h"

versions_3h <- read.table("data_versions/statistics_versions_3h.csv",
                        header = TRUE)

versions_3h$tres <- "03h"

versions_12h <- read.table("data_versions/statistics_versions_12h.csv",
                         header = TRUE)

versions_12h$tres <- "12h"

all.versions <- rbind(versions_1h, versions_3h, versions_12h)
all.versions$TOD <- ordered(all.versions$TOD, levels = c("day", "twilight", "night"))

all.vrsns <- data.frame(values = c(all.versions$R2, all.versions$RMSE, 
                              all.versions$MAE, all.versions$ME),
                   stat = c(rep("Rsq.", nrow(all.versions)), 
                            rep("RMSE", nrow(all.versions)), 
                            rep("MAE", nrow(all.versions)), 
                            rep("ME", nrow(all.versions))), 
                   tod = all.versions$TOD, tres = all.versions$tres)

all.vrsns$stat <- ordered(all.vrsns$stat, levels = c("Rsq.", "RMSE", "MAE", "ME"))
all.vrsns$tod <- ordered(all.vrsns$tod, levels = c("day", "twilight", "night"))

all.vrsns.stats <- ggplot(all.vrsns, aes(tod, values)) +
  facet_grid(stat ~ tres, as.table = TRUE, scales = "free_y", 
             labeller = "label_value") + 
  geom_boxplot(aes(fill = tod), outlier.shape = "*",
               outlier.size = 3) +
  scale_fill_manual(values = fill.clrs, name = "Time of day", 
                    limits = levels(all.scns$tod)) +
  scale_x_discrete(name = "", limits = levels(all.scns$tod)) +
  scale_y_continuous(name = "")

pdf("data_versions/bplot_versions.pdf", 
    height = 10, width = 8)

print(all.vrsns.stats)

dev.off()

png("data_versions/bplot_versions.png", 
    height = 1024 * 3, width = 748 * 3, res = 300)

print(all.vrsns.stats)

dev.off()

png("data_versions/bplot_versions_bw.png", 
    height = 1024 * 3, width = 748 * 3, res = 300)

print(all.vrsns.stats + theme_bw())

dev.off()

#### boxplot over 24h ##########################################################

data_24h <- read.table("data_24h/scores_24-7.dat",
                          header = TRUE)

data_24h$hr <- sprintf("%02.f", as.numeric(substr(data_24h$chDate, 10, 11)) + 1)
tod <- vector("character", nrow(data_24h))
ind.night <- which(as.numeric(data_24h$hr) < 3 | as.numeric(data_24h$hr) > 20)
ind.day <- which(as.numeric(data_24h$hr) > 5 & as.numeric(data_24h$hr) < 17)
tod[ind.night] <- "night"
tod[ind.day] <- "day"
tod[tod == ""] <- "twilight"

data_24h$TOD <- tod
data_24h$TOD <- ordered(data_24h$TOD, labels = labs)

mean.r2 <- mean(data_24h$R2, na.rm = TRUE)
sd.r2 <- sd(data_24h$R2, na.rm = TRUE)
data_24h$R2 <- ifelse(data_24h$R2 > mean.r2 + 2 * sd.r2 | 
                        data_24h$R2 < mean.r2 - 2 * sd.r2,
                      NA, data_24h$R2)

mean.rmse <- mean(data_24h$RMSE, na.rm = TRUE)
sd.rmse <- sd(data_24h$RMSE, na.rm = TRUE)
data_24h$RMSE <- ifelse(data_24h$RMSE > mean.rmse + 2 * sd.rmse | 
                        data_24h$RMSE < mean.rmse - 2 * sd.rmse,
                      NA, data_24h$RMSE)

mean.mae <- mean(data_24h$MAE, na.rm = TRUE)
sd.mae <- sd(data_24h$MAE, na.rm = TRUE)
data_24h$MAE <- ifelse(data_24h$MAE > mean.mae + 2 * sd.mae | 
                        data_24h$MAE < mean.mae - 2 * sd.mae,
                      NA, data_24h$MAE)

mean.me <- mean(data_24h$ME, na.rm = TRUE)
sd.me <- sd(data_24h$ME, na.rm = TRUE)
data_24h$ME <- ifelse(data_24h$ME > mean.me + 2 * sd.me | 
                        data_24h$ME < mean.me - 2 * sd.me,
                      NA, data_24h$ME)

all.24 <- data.frame(values = c(data_24h$R2, data_24h$RMSE, 
                                  data_24h$MAE, data_24h$ME),
                       stat = c(rep("Rsq.", nrow(data_24h)), 
                                rep("RMSE", nrow(data_24h)), 
                                rep("MAE", nrow(data_24h)), 
                                rep("ME", nrow(data_24h))), 
                       tod = data_24h$TOD, hr = data_24h$hr)

all.24$stat <- ordered(all.24$stat, levels = c("Rsq.", "RMSE", "MAE", "ME"))
all.24$tod <- ordered(all.24$tod, levels = c("day", "twilight", "night"))

w <- sqrt(table(all.24$tod)/nrow(all.24))


all.24.stats <- ggplot(all.24, aes(hr, values)) +
  facet_grid(stat ~ ., as.table = TRUE, scales = "free_y", 
             labeller = "label_value") +   
  llply(unique(all.24$tod), 
        function(i) geom_boxplot(aes(fill = tod), 
                                 width = w[i], outlier.shape = "*",
                                 outlier.size = 3,
                                 data = subset(all.24, tod == i))) +
  scale_fill_manual(values = fill.clrs, name = "Time of day",
                    labels = levels(all.scns$tod)) +
  scale_x_discrete(name = "Hour of day (UTC)") +
  scale_y_continuous(name = "")

pdf("data_24h/bplot_24h.pdf", 
    height = 10, width = 8)

print(all.24.stats)

dev.off()

png("data_24h/bplot_24h.png", 
    height = 1024 * 3, width = 748 * 3, res = 300)

print(all.24.stats)

dev.off()

png("data_24h/bplot_24h_bw.png", 
    height = 1024 * 3, width = 748 * 3, res = 300)

print(all.24.stats + theme_bw())

dev.off()

################################################################################
################################################################################
################################################################################


############## lattice approach ################################################
# library(latticeExtra)
# 
# # opar <- trellis.par.get()
# # trellis.par.set(theEconomist.theme(box = "transparent"))
# # oopt <- lattice.options(theEconomist.opts())
# 
# mytheme <- custom.theme(fill = fill.clrs
# 
# r2.all <- bwplot(R2 ~ TOD | tres, data = all.scenes, varwidth = TRUE, 
#                  between = list(x = 0.4, y = 0), panel = function(...) {
#                    panel.grid(v = 0, h = -1, lty = 2, col.line = "grey80")
#                    panel.bwplot(...)
#                  }, layout = c(3, 1))
# 
# rmse.all <- bwplot(RMSE ~ TOD | tres, data = all.scenes, varwidth = TRUE,  
#                    between = list(x = 0.4, y = 0), panel = function(...) {
#                      panel.grid(v = 0, h = -1, lty = 2, col.line = "grey80")
#                      panel.bwplot(...)
#                    }, layout = c(3, 1))
# 
# mae.all <- bwplot(MAE ~ TOD | tres, data = all.scenes, varwidth = TRUE, 
#                   between = list(x = 0.4, y = 0), panel = function(...) {
#                     panel.grid(v = 0, h = -1, lty = 2, col.line = "grey80")
#                     panel.bwplot(...)
#                   }, layout = c(3, 1))
# 
# me.all <- bwplot(ME ~ TOD | tres, data = all.scenes, varwidth = TRUE, 
#                  between = list(x = 0.4, y = 0), panel = function(...) {
#                    panel.grid(v = 0, h = -1, lty = 2, col.line = "grey80")
#                    panel.bwplot(...)
#                  }, layout = c(3, 1))
# 
# 
# masterLayout <- grid.layout(
#   nrow = 4, ncol = 1,
#   heights = 0.23,
#   widths = 0.95,
#   default.units = "npc",
#   respect = FALSE)
# 
# vp1 <- viewport(layout.pos.row=1,  name="vp1")  
# vp2 <- viewport(layout.pos.row=2,  name="vp2")     
# vp3 <- viewport(layout.pos.row=3,  name="vp3")  
# vp4 <- viewport(layout.pos.row=4,  name="vp4")  
# 
# pdf("/media/windows/tappelhans/uni/marburg/colleagues/meike/randomforest/boxplots/data_scenes/bplot_scenes_lattice.pdf", height = 11, width = 8)
# 
# grid.newpage()
# pushViewport(vpTree(viewport(layout = masterLayout,name="master"), vpList(vp1, vp2, vp3, vp4)))
# seekViewport("master")
# print(update(r2.all, strip = strip.custom(
#   bg = "grey40", par.strip.text = list(col = "white", 
#                                        font = 2)),
#   par.settings = list(
#   plot.symbol = list(pch = 4, col = "grey30", cex = 0.5), 
#   box.umbrella = list(lty = 1, col = "grey30"),
#   box.dot = list(col = "black", pch = "|", cex = 1.5), 
#   box.rectangle = list(col = "grey30")), fill =  brewer.pal(3, "Blues"), 
#              box.ratio = 1), draw.in = "vp1")
# print(update(rmse.all, strip = strip.custom(
#   bg = "grey40", par.strip.text = list(col = "white", 
#                                        font = 2)),
#       par.settings = list(
#         plot.symbol = list(pch = 4, col = "grey30", cex = 0.5), 
#         box.umbrella = list(lty = 1, col = "grey40"),
#         box.rectangle = list(col = "grey40")), fill =  brewer.pal(3, "Blues"),
#       pch = "|"), draw.in = "vp2")
# print(update(mae.all, strip = strip.custom(
#   bg = "grey40", par.strip.text = list(col = "white", 
#                                        font = 2)),
#       par.settings = list(
#         plot.symbol = list(pch = 4, col = "grey30", cex = 0.5), 
#         box.umbrella = list(lty = 1, col = "grey40"),
#         box.rectangle = list(col = "grey40")), fill =  brewer.pal(3, "Blues"),
#       pch = "|"), draw.in = "vp3")
# print(update(me.all, strip = strip.custom(
#   bg = "grey40", par.strip.text = list(col = "white", 
#                                        font = 2)),
#       par.settings = list(
#         plot.symbol = list(pch = 4, col = "grey30", cex = 0.5), 
#         box.umbrella = list(lty = 1, col = "grey40"),
#         box.rectangle = list(col = "grey40")), fill =  brewer.pal(3, "Blues"),
#       pch = "|"), draw.in = "vp4")
# 
# dev.off()

######### ALTERNATE VERSION FOR SCENES #########################################


# all.r2 <- ggplot(all.scenes, aes(TOD, R2)) + 
#   facet_grid(. ~ tres) + 
#   llply(unique(all.scenes$TOD), 
#         function(i) geom_boxplot(aes(fill = TOD), 
#                                  width = w[i], 
#                                  data = subset(all.scenes, TOD == i))) +
#   scale_fill_manual(values = fill.clrs) +
#   scale_x_discrete(name = "") 
# 
# all.r2 <- all.r2 +
#   scale_y_continuous(name = expression(R^2), 
#                      breaks = ggplot_build(all.r2)$panel$ranges[[1]]$y.major_source,
#                      labels = sprintf("%0.2f", 
#                                       ggplot_build(all.r2)$panel$ranges[[1]]$y.major_source))
# 
# all.rmse <- ggplot(all.scenes, aes(TOD, RMSE)) + 
#   facet_grid(. ~ tres) + 
#   geom_boxplot(aes(fill = TOD)) +
#   scale_fill_manual(values = fill.clrs) +
#   scale_x_discrete(name = "") 
# 
# all.rmse <- all.rmse +
#   scale_y_continuous(name = expression(RMSE^""), 
#                      breaks = ggplot_build(all.rmse)$panel$ranges[[1]]$y.major_source,
#                      labels = sprintf("%0.2f", 
#                                       ggplot_build(all.rmse)$panel$ranges[[1]]$y.major_source))
# 
# all.mae <- ggplot(all.scenes, aes(TOD, MAE)) + 
#   facet_grid(. ~ tres) + 
#   geom_boxplot(aes(fill = TOD)) +
#   scale_fill_manual(values = fill.clrs) +
#   scale_x_discrete(name = "")
# 
# all.mae <- all.mae +
#   scale_y_continuous(breaks = ggplot_build(all.mae)$panel$ranges[[1]]$y.major_source,
#                      labels = sprintf("%0.2f", 
#                                       ggplot_build(all.mae)$panel$ranges[[1]]$y.major_source))
# 
# all.me <- ggplot(all.scenes, aes(TOD, ME)) + 
#   facet_grid(. ~ tres) + 
#   geom_boxplot(aes(fill = TOD)) +
#   scale_fill_manual(values = fill.clrs) +
#   scale_x_discrete(name = "") 
# 
# all.me <- all.me +
#   scale_y_continuous(breaks = ggplot_build(all.me)$panel$ranges[[1]]$y.major_source,
#                      labels = sprintf("%0.2f", 
#                                       ggplot_build(all.me)$panel$ranges[[1]]$y.major_source))
# 
# masterLayout <- grid.layout(
#   nrow = 4, ncol = 1,
#   heights = 0.23,
#   widths = 0.95,
#   default.units = "npc",
#   respect = FALSE)
# 
# pdf("data_scenes/bplot_scenes.pdf", height = 11, width = 8)
# 
# grid.newpage()
# 
# pushViewport(viewport(layout = masterLayout))
# print(all.r2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(all.rmse, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# print(all.mae, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
# print(all.me, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
# 
# dev.off()

########## ALTERNATE VERSION FOR VERSIONS ######################################


# all.r2 <- ggplot(all.versions, aes(TOD, R2)) + 
#   facet_grid(. ~ tres) + 
#   geom_boxplot(aes(fill = TOD)) +
#   scale_fill_manual(values = fill.clrs) +
#   scale_x_discrete(name = "") 
# 
# all.r2 <- all.r2 +
#   scale_y_continuous(name = expression(R^2), 
#                      breaks = ggplot_build(all.r2)$panel$ranges[[1]]$y.major_source,
#                      labels = sprintf("%0.2f", 
#                                       ggplot_build(all.r2)$panel$ranges[[1]]$y.major_source))
# 
# all.rmse <- ggplot(all.versions, aes(TOD, RMSE)) + 
#   facet_grid(. ~ tres) + 
#   geom_boxplot(aes(fill = TOD)) +
#   scale_fill_manual(values = fill.clrs) +
#   scale_x_discrete(name = "") 
# 
# all.rmse <- all.rmse +
#   scale_y_continuous(name = expression(RMSE^""), 
#                      breaks = ggplot_build(all.rmse)$panel$ranges[[1]]$y.major_source,
#                      labels = sprintf("%0.2f", 
#                                       ggplot_build(all.rmse)$panel$ranges[[1]]$y.major_source))
# 
# all.mae <- ggplot(all.versions, aes(TOD, MAE)) + 
#   facet_grid(. ~ tres) + 
#   geom_boxplot(aes(fill = TOD)) +
#   scale_fill_manual(values = fill.clrs) +
#   scale_x_discrete(name = "")
# 
# all.mae <- all.mae +
#   scale_y_continuous(breaks = ggplot_build(all.mae)$panel$ranges[[1]]$y.major_source,
#                      labels = sprintf("%0.2f", 
#                                       ggplot_build(all.mae)$panel$ranges[[1]]$y.major_source))
# 
# all.me <- ggplot(all.versions, aes(TOD, ME)) + 
#   facet_grid(. ~ tres) + 
#   geom_boxplot(aes(fill = TOD)) +
#   scale_fill_manual(values = fill.clrs) +
#   scale_x_discrete(name = "") 
# 
# all.me <- all.me +
#   scale_y_continuous(breaks = ggplot_build(all.me)$panel$ranges[[1]]$y.major_source,
#                      labels = sprintf("%0.2f", 
#                                       ggplot_build(all.me)$panel$ranges[[1]]$y.major_source))
# 
# masterLayout <- grid.layout(
#   nrow = 4, ncol = 1,
#   heights = 0.23,
#   widths = 0.95,
#   default.units = "npc",
#   respect = FALSE)
# 
# pdf("data_versions/bplot_versions.pdf", height = 11, width = 8)
# 
# grid.newpage()
# 
# pushViewport(viewport(layout = masterLayout))
# print(all.r2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(all.rmse, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# print(all.mae, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
# print(all.me, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
# 
# dev.off()

########## ALTERNATE VERSION FOR 24H ###########################################


# d24.r2 <- ggplot(data_24h, aes(hr, R2)) +
#   llply(labs, 
#         function(i) geom_boxplot(aes(fill = TOD),
#                                  width = w[i], outlier.shape = "*", 
#                                  line.size = 0.5, fatten = 1.5,
#                                  data = subset(data_24h, TOD == i))) +
#   scale_fill_manual(values = fill.clrs, labels = labs) +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = expression(R^2)) #+
#   #theme_bw()
# 
# d24.rmse <- ggplot(data_24h, aes(hr, RMSE)) + 
#   llply(labs, function(i) geom_boxplot(aes(fill = TOD), 
#                                        width = w[i], outlier.shape = "*", 
#                                        line.size = 0.5, fatten = 1.5,
#                                        data = subset(data_24h, TOD == i))) +
#   scale_fill_manual(values = fill.clrs, labels = labs) +
#   scale_x_discrete(name = "") #+
# #  theme_bw()
# 
# d24.mae <- ggplot(data_24h, aes(hr, MAE)) + 
#   llply(labs, function(i) geom_boxplot(aes(fill = TOD), 
#                                        width = w[i], outlier.shape = "*", 
#                                        line.size = 0.5, fatten = 1.5,
#                                        data = subset(data_24h, TOD == i))) +
#   scale_fill_manual(values = fill.clrs, labels = labs) +
#   scale_x_discrete(name = "") #+
# #  theme_bw()
# 
# d24.me <- ggplot(data_24h, aes(hr, ME)) + 
#   llply(labs, function(i) geom_boxplot(aes(fill = TOD), 
#                                        width = w[i], outlier.shape = "*", 
#                                        line.size = 0.5, fatten = 1.5,
#                                        data = subset(data_24h, TOD == i))) +
#   scale_fill_manual(values = fill.clrs, labels = labs) +
#   scale_x_discrete(name = "") #+
# #  theme_bw()
# 
# masterLayout <- grid.layout(
#   nrow = 4, ncol = 1,
#   heights = 0.23,
#   widths = 0.95,
#   default.units = "npc",
#   respect = FALSE)
# 
# pdf("data_24h/bplot_24h.pdf", height = 11, width = 8)
# #png("data_24h/bplot_24h.png", 
# #    height = 1024 * 8, width = 768 * 8, res = 600)
# 
# grid.newpage()
# 
# pushViewport(viewport(layout = masterLayout))
# print(d24.r2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# print(d24.rmse, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# print(d24.mae, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
# print(d24.me, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
# 
# dev.off()
# 
