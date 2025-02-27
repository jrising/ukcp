setwd("~/Dropbox/UK Economic Risks")

incs <- read.csv("socioeconomics/adm3inc.csv")
incs$GDP <- as.numeric(incs$GDPpc) * incs$pop

channel = "Agriculture"

filenames <- list("SLR"="adm3combo.RData", "Energy-Supply"="adm3combo.RData", "Energy-Demand"="adm3combo.RData",
                  "Labor"="adm3combo.RData", "Agriculture"="agcombo-adm3.RData", "Riverine"="rivercombo-adm3.RData")

library(dplyr)

load(file.path("~/Dropbox/COACCH/channels", channel, filenames[[channel]]))
combo2 <- combo %>% left_join(incs, by='county')
combo2$percent <- 100 * combo2$damage.mgbp * 1e6 / combo2$GDP

source("~/Dropbox/UK Economic Risks/lib/report.R")
source("~/Dropbox/UK Economic Risks/lib/constants.R")

shp.adm3 <- importShapefile("~/Dropbox/UK Economic Risks/regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
shp.adm3.polydata <- attr(shp.adm3, 'PolyData')

quantile(combo2$percent, c(.05, .95))

limits <- list("Energy-Demand"=c(0, 2.5), "Energy-Supply"=c(0, .005), "Labor"=c(0, 3),
               "SLR"=c(-0.1, 0.7), "Agriculture" = c(-.2, .3), "Riverine"=c(0,1.20))

setwd(file.path("~/Dropbox/COACCH/channels", channel))

make.maps.loop("graphs", "combo", combo2, shp.adm3, function(subres) {
  subres %>% group_by(PID) %>% summarize(mu=mean(percent, na.rm=T), ci5=quantile(percent, .05, na.rm=T),
                                         ci95=quantile(percent, .95, na.rm=T))
}, "Damages\n(% GDP)", "95% CI\ndamage\n(% GDP)", limits[[channel]][1], limits[[channel]][2])
