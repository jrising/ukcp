setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

load("channels/health/combo-adm3.RData")

source("lib/report.R")

shp <- importShapefile("regions/gadm36_GBR_shp/gadm36_GBR_3.shp")
polydata <- attr(shp, 'PolyData')

library(dplyr)
combo2 <- combo %>% left_join(polydata[, c('PID', 'NAME_3')], by=c('county'='NAME_3'))

make.maps.loop("channels/health/figures", "combo", combo2, shp, function(subres) {
    subres %>% group_by(PID) %>% summarize(mu=mean(deathrate, na.rm=T), ci5=quantile(deathrate, .05, na.rm=T), ci95=quantile(deathrate, .95, na.rm=T))
}, "Expected\ndeathrate\nchange\n(per 100k)", "95% CI\ndeathrate\nchange\n(per 100k)", -1, 10)
